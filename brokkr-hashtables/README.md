# Brokkr-HashTables

This package provides flexible mutable hash tables. It supports different storage backends for both keys and values, which can significantly change the performance characteristics of the table.

It does not require a specific hash function. Instead, a user is asked to provide their own, to more accurately match the performance/distribution trade-off a user needs.

## Quickstart

The following snippet creates a HashTable that stores keys using `Foreign.Storable` and values as lifted Haskell heap objects.

```haskell
{-# LANGUAGE DataKinds #-}
import Brokkr.HashTable qualified as HT
import Control.Monad.ST (RealWorld)
import Foreign.Storable

type HashTable k v = HT.HashTable' HT.Storable HT.Boxed k v

newtype IntIdHash = IntIdHash Int
  deriving newtype (Eq, Storable)

instance HT.Hash IntIdHash where
  hash (IntIdHash x) = HT.HashFn (const x)

foo :: IO (HashTable RealWorld IntIdHash String)
foo = do
  t <- HT.new initialSalt maxLoadFactor
  HT.insert t (IntIdHash 1) "Hello"
  HT.lookup t (IntIdHash 1)
  HT.delete t (IntIdHash 1)
  pure t
  where
    maxLoadFactor = 0.75
    -- Normally this would be from some random source
    -- but since the hash here doesn't use it, it does not matter
    initialSalt = 0 
```

> The hash function used here ignores the salt and only works well if the integer keys are somewhat uniform. Use the right hash function for your data!

## Documentation

It is recommended to use qualified imports and define a type alias for the specific key value backend that a table is supposed to use:

```haskell
type HashTable k v = HT.HashTable' keyStorage valueStorage k v
```

The different available storage kinds for both keys and values are:
```haskell
data Storage = Boxed | Storable | Prim
```
The effects of choosing which storage backend are mostly performance related and are discussed below.

Interacting with a table is focussed around a small set of functions with fairly obvious semantics:
* `insert`/`insertWithHash`
* `lookup`/`lookupWithHash`
* `delete`/`deleteWithHash`

> The `*-WithHash` functions are provided to allow working with precomputed hashes, or avoiding the `Hash` typeclass, without baking this into the key datatype.

Creating a table is done through `new` and can be passed an initial salt for the hash and a max load factor.

Iterating the table is done through `foldM`.

When inserting a lot of elements into a table `reserve` can be used to resize the table to an appropriate size to delay or even omit otherwise necessary and expensive growth steps.

### Performance trade-offs and selecting the right table structure

If you want to select the fastest table for your application, read this section carefully. All options are reasonably fast, but some do better in certain scenarios. As a rule of thumb: Use the `Storable` or `Prim` instances if you can.

#### Hash function

Since the `Hash` typeclass has no instances, you are required to provide one such instance. These can range from using the value of integer keys as their hash to cryptographic hashes. Whatever you can hash to word sized integers works. [^1] However, make sure the hash function has a decently uniform distribution otherwise performance will suffer!

>[!WARNING]
> Besides allowing a random salt on creation, a table itself offers zero additional denial of service protection. A table will degrade in performance for too many collisions. Also, if too many collisions happen to one key, it will also resize and allocate more memory! Use a strong hash function should you be concerned about this angle of attack!

`hashable` can also be used with a newtype:
```haskell
newtype ViaHashable a = ViaHashable a
instance Hashable a => HT.Hash (ViaHashable a) where
  hash (ViaHashable x) = HashFn $ \s -> hashWithSalt s x
```

[^1]: Technically the table will use at most the low `k`-bits of the hash, where `k = log_2(table_capacity)`, so 32bit hash functions will work just as well as 64bit versions, so long as the hash itself is good (and you are not inserting more than `2^32`/`2^64` elements).

#### Key choice

Besides a good and fast hash function for the key, a fast equality check can improve performance.

A structure similar to `hashable`'s `Hashed` can be used to provide fast inequality checks if failing lookups are common. This may degrade insert performance due to more allocations, so benchmark!

#### Lookups

The fastest `lookup` operations are always with `Storable` or `Prim` keys. For lookups specifically the value storage hardly matters. There are some cases where `Storable` value storage has a slight edge, as it can sometimes avoid the extra memory load that both `Prim` and `Boxed` require. On very large tables `Storable` keys ever so slightly beat `Prim` keys.

A high max load factor can decrease the `lookup` performance, but at least in the benchmarks in this package (which compare `0.75` and `0.9`), the difference was not significant.[^2]
[^2]: It is my assumption that robin-hood linear probing does a decent job of breaking up what would otherwise be long linear probing chains, which is why the load factor matters less here.

#### Inserts

> Doing bulk inserts? Make sure to always `reserve` enough space beforehand, it can lead to massive performance improvements.

The fastest `insert` operations are on either `Storable` or `Prim` based storage for both keys and values, though keys are more important. This has a couple of reasons:
- GHC often unboxes boxed elements, so insert has to re-box/allocate them before writing to the array
- Resizing/Growing allocates new arrays. For boxed components GHC has to fill those with some default, which is linear in its size
- Inserts first need to find an available slot. This involves key comparisons, which means needlessly following a pointer to the actual key data, *if* a `Storable` or `Prim` instance had been possible

Either `Prim` or `Storable` is usually fine, but the `Storable` instance will be slightly faster the larger the table gets.

The max table load factor is another factor to consider. Generally the higher the load factor the slower are all operations (besides folding that is). On very large tables it merely delays resizes slightly and has little further impact on performance. A value of `0.75` is a decent default, but feel free to experiment with anything between 0-1 (not inclusive: 0 has obvious problems. 1 can lead to inserts shifting the whole table).

<details>

<summary>When will a table resize?</summary>

There are two reasons a table chooses to grow:
* The max load factor is exceeded
* One collision chain for one hash is too long. The maximum length for the chain depends on the capacity of the table. Currently, it is `log_2` of the table capacity. This together with robin-hood linear probing is what makes this table fast. Having a max length enables the lookup function to be a tight loop with just two branches (one for metadata, one for the key test).

</details>

### In memory representation and overhead

All tables store only the key and value plus one byte of metadata. For `Boxed` and `Prim` backed storage these components are all in separate arrays.
* `Boxed` is stored in a `MutableArray#`
* `Storable` is stored in an aligned pinned `MutableByteArray#`
* `Prim` is stored in a not explicitly pinned [^pin] `MutableByteArray#`, *if* the alignment fits the word alignment (usually 4 or 8 for 32 or 64-bit systems). If it requires different alignment, an aligned and pinned `MutableByteArray#` is used instead. 

[^pin]: GHC will automatically pin any heap object beyond a certain size regardless of the primitive used.

`Storable` sometimes behaves slightly different with more overhead:

- Should only the values be `Storable`, they are in their own array with no additional overhead.
- If only keys use `Storable`, the values are in their own array, but the keys are stored together with the one byte metadata. Alignment then often requires extra padding. For example using 64-bit keys with an 8 byte alignment would add 7 bytes of padding between metadata and key. This trades space for performance, table operations benefit from dealing with just one array, with the benefits growing larger with larger tables.
- Additionally, if both keys and values are `Storable`, all elements are stored in one array. This again may require padding, but has similar benefits.

> The benefits of using `Storable` keys and values compared to `Prim` exist, but are admittedly small until the table grows huge. As always benchmark for your use case, changing table options should be trivial.

### Comparisons to existing Haskell hashtable libraries

The performance is judged based on comparison benchmarks. The overhead and overall api by both the source and documentation. I may or may not remember to keep this up to date if any of the libraries update in the future. The benchmarks were run on a ryzen 7950x, depending on cache sizes and other architectural differences you may have different results (if so please tell me!).

Statements such as "slow xxx" or "more/less overhead" are made relative to this library. 

#### hashtables

Pro:

- Variety of different table implementations
- Prime number based growth. Grows slower than powers of 2.[^4]

Con:

- Slow lookups and inserts for all tables
- Slightly more overhead per key (ignoring `Storable` tables and their padding)
- No option of unboxed/flat storage. This means more gc pressure, more allocations and more memory overhead.
- No options to manipulate the seed of the hash function or the hash function itself. `hashable` is used.
- No options to tweak the max load factor
- No option to `reserve` space beyond the initial creation size

#### vector-hashtables

Pro:

- Cheaper resizes at huge table sizes due to a different storage approach
- Immutable api
- Fast failing lookups for `Boxed` components on small to medium-sized tables, because it stores hashes and compares against those first, providing a cheaper inequality check. This advantage vanishes when keys are `Storable` or `Prim` and have a cheap comparison function. And even for `Boxed` this effect vanishes for larger tables as their lookup performance degrades for both successful and failing lookups. [^3]
- Prime number based growth. Grows slower than powers of 2.[^4]

Con:

- Slow lookups for all tables (except for `Boxed` keys). Lookups degrade far worse with table size than in `brokkr-hashtables`, where they are basically constant. [^3]
- Slower inserts until the tables grow very large. There `Storable` and `Prim` based tables perform on par with all `vector-hashtables` variants (sometimes even outperforming them, especially with `reserve`), but for `Boxed` versions `vector-hashtables` has far better insert (and more importantly growth) behavior on very large tables.
- Much higher overhead per key value pair: 3 word sized integers per key value pair in the table.
- No options to manipulate the seed of the hash function or the hash function itself. `hashable` is used.
- No options to tweak the max load factor
- No option to `reserve` space beyond the initial creation size

[^3]: If I had to guess, I'd blame their collision strategy. It's effectively a linked list per bucket, but the list is encoded as a flat array and shared among all buckets. Clever, as long as it fits cache and entries stay close, but a nightmare of memory loads for larger tables.

[^4]: While they both have a predefined list of primes, both libraries miss out on optimizing the remainder by constant operation performed on every lookup/insert/delete.

<--- TODO: As a reminder for me. Compile the benchmarks into one graph to clearly show differences in less vague terms --->

## Credits

ska:flat_hash_map: https://github.com/skarupke/flat_hash_map

This library is part of the ecosystem supporting `Brokkr` a Minecraft server written in Haskell.
