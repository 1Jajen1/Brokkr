# Brokkr-HashTables

This package provides flexible mutable hash tables. It supports different storage backends for both keys and values, which can significantly change the performance characteristics of the table.

It does not require a specific hash function. Instead, a user is asked to provide their own, to more accurately match the performance/distribution trade-off a user needs.

<!-- MarkdownTOC autolink="true" -->

- [Quickstart](#quickstart)
- [Documentation](#documentation)
- [Performance trade-offs and selecting the right table structure](#performance-trade-offs-and-selecting-the-right-table-structure)
    - [Hash function](#hash-function)
    - [Key choice](#key-choice)
    - [Lookups](#lookups)
    - [Modification](#modification)
- [Memory representation and overhead](#memory-representation-and-overhead)
- [Comparisons to existing Haskell hashtable libraries](#comparisons-to-existing-haskell-hashtable-libraries)
    - [Benchmarks](#benchmarks)
        - [Lookups](#lookups-1)
        - [Insert + Delete](#insert--delete)
        - [Bulk inserts](#bulk-inserts)
    - [Conclusions](#conclusion)
        - [hashtables](#hashtables)
        - [vector-hashtables](#vector-hashtables)
        - [IntMap/HashMap](#intmaphashmap)
- [Credits](#credits)


## Quickstart

The following snippet creates a HashTable that stores keys using `Foreign.Storable` and values as lifted Haskell heap objects.

```haskell
{-# LANGUAGE DataKinds #-}
import Brokkr.HashTable qualified as HT
import Control.Monad.ST (RealWorld)
import Foreign.Storable

type HashTable s k v = HT.HashTable' HT.Storable HT.Boxed s k v

newtype IntIdHash = IntIdHash Int
  deriving newtype (Eq, Storable)

instance HT.Hash IntIdHash where
  hash (IntIdHash x) = HT.HashFn (const x)

foo :: IO (HashTable RealWorld IntIdHash String)
foo = do
  t <- HT.new 8 initialSalt maxLoadFactor
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
type HashTable s k v = HT.HashTable' keyStorage valueStorage s k v
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

Creating a table is done through `new` and can be passed an initial size, a salt for the hash and a max load factor.

Iterating the table is done through `foldM`.

> Remember to not modify the table during iteration!

When inserting a lot of elements into a table `reserve` can be used to resize the table to an appropriate size to delay or even omit otherwise necessary and expensive growth steps.

## Performance trade-offs and selecting the right table structure

If you want to select the fastest table for your application, read this section carefully. All options are reasonably fast, but some do better in certain scenarios. As a rule of thumb: Use the `Storable` or `Prim` instances if you can.

### Hash function

Since the `Hash` typeclass has no instances, you are required to provide one such instance. These can range from using the value of integer keys as their hash, to cryptographic hashes. Whatever you can hash to word sized integers works. [^1] However, make sure the hash function has a decently uniform distribution otherwise performance will suffer!

>[!WARNING]
> Besides allowing a random salt on creation, a table itself offers zero additional denial of service protection. A table will degrade in performance for too many collisions. Also, if too many collisions happen to one key, it will also resize and allocate more memory! Use a strong hash function should you be concerned about this angle of attack!

`hashable` can also be used with a newtype:
```haskell
newtype ViaHashable a = ViaHashable a
instance Hashable a => HT.Hash (ViaHashable a) where
  hash (ViaHashable x) = HashFn $ \s -> hashWithSalt s x
```

[^1]: Technically the table will use at most the low `k`-bits of the hash, where `k = log_2(table_capacity)`, so 32bit hash functions will work just as well as 64bit versions, so long as the hash itself is good (and you are not inserting more than `2^32`/`2^64` elements).

### Key choice

Besides a good and fast hash function for the key, a fast equality check can improve performance.

A structure similar to `hashable`'s `Hashed` can be used to provide fast inequality checks if failing lookups are common. This may degrade performance due to more allocations depending on the storage used, so benchmark!

### Lookups

The fastest `lookup` operations are always with `Storable` or `Prim` keys. For lookups specifically the value storage hardly matters. There are some cases where `Storable` value storage has a slight edge, as it can sometimes avoid the extra memory load that both `Prim` and `Boxed` require.

A high max load factor can decrease the `lookup` performance, but at least in the benchmarks in this package (which compare `0.75` and `0.9`), the difference was not significant.[^2]
[^2]: It is my assumption that robin-hood linear probing does a decent job of breaking up what would otherwise be long linear probing chains, which is why the load factor matters less here.

### Modification

> Doing bulk inserts? Make sure to always `reserve` enough space beforehand. Table rehashes are very expensive.

The fastest `insert` and `delete` operations are on either `Storable` or `Prim` based storage for both keys and values, though keys are more important. This has a couple of reasons:
- GHC often unboxes boxed elements, so `insert` has to re-box/allocate them before writing to the array
- `Storable` tables store all elements in one array. This means reads and writes are often in cache.
- Inserts first need to find an available slot. This involves key comparisons, which means needlessly following a pointer to the actual key data, *if* a `Storable` or `Prim` instance had been possible

Either `Prim` or `Storable` is usually fine, but the `Storable` instance will be faster the larger the table gets.

The max table load factor is another factor to consider. Generally the higher the load factor the slower all operations are (besides folding that is). On very large tables it merely delays resizes slightly and has little further impact on performance. A value of `0.75` is a decent default, but feel free to experiment with anything between 0-1 (not inclusive: 0 has obvious problems. 1 can lead to inserts shifting the whole table).

<details>

<summary>Note: When will a table resize?</summary>

There are two reasons a table chooses to grow:
* The max load factor is exceeded
* One collision chain for one hash is too long. The maximum length for the chain depends on the capacity of the table. Currently, it is `log_2` of the table capacity. This together with robin-hood linear probing is what makes this table fast. Having a max length enables the lookup function to be a tight loop with just two branches (one for metadata, one for the key test).

</details>

## Memory representation and overhead

All tables store only the key and value plus one byte of metadata. For `Boxed` and `Prim` backed storage these components are all in separate arrays.
* `Boxed` is stored in a `MutableArray#`
* `Storable` is stored in an aligned pinned `MutableByteArray#`
* `Prim` is stored in a not explicitly pinned [^pin] `MutableByteArray#`, *if* the alignment fits the word alignment (usually 4 or 8 for 32 or 64-bit systems). If it requires different alignment, an aligned and pinned `MutableByteArray#` is used instead. 

[^pin]: GHC will automatically pin any heap object beyond a certain size regardless of the primitive used.

`Storable` sometimes behaves slightly different with more overhead:

- Should only the values be `Storable`, they are in their own array with no additional overhead.
- If only keys use `Storable`, the values are in their own array, but the keys are stored together with the one byte metadata. Alignment then often requires extra padding. For example using 64-bit keys with an 8 byte alignment would add 7 bytes of padding between metadata and key. This trades space for performance, table operations benefit from dealing with just one array, with the benefits growing larger with larger tables.
- Additionally, if both keys and values are `Storable`, all elements are stored in one array. This again may require padding, but has similar benefits.

> The benefits of using `Storable` keys and values compared to `Prim` exist mainly on inserts and deletes, but are admittedly small until the table grows huge. As always benchmark for your use case, changing tables storage should be trivial.

## Comparisons to existing Haskell hashtable libraries

The performance is judged based on comparison benchmarks. The memory overhead and overall api by both the source and documentation. I may or may not remember to keep this up to date if any of the libraries update in the future. The benchmarks were run on a ryzen 7950x, depending on cache sizes and other architectural differences you may have different results (if so please tell me!).

### Benchmarks

Bold numbers highlight the fastest for their respective category. `HashMap` is put into boxed keys and values. `IntMap` has prim keys and boxed values.

#### Lookups

`Int` keys, using the int value as the hash. **Valid** lookups. Each run looks up one thousand valid keys from a table with the given size. Values are always boxed. The load factor for this libraries tables is always `0.75`.

| Name                                |   25   |   50   |   100  |  1_000 |  10_000 | 100_000 | 1_000_000 | 10_000_000 |
|-------------------------------------|--------|--------|--------|--------|---------|---------|-----------|------------|
| `brokkr-hashtables` (boxed    keys) | **2.83μs** | **3.60μs** | **4.53μs** | **2.69μs** |  **3.16μs** |  **2.61μs** |    **3.63μs** |     **3.56μs** |
| `vector-hashtables` (boxed    keys) | 4.74μs | 4.51μs | **4.57μs** | 4.72μs |  5.97μs |  7.09μs |    6.43μs |     7.05μs |
| `hashtables` (Basic)                | 6.29μs | 6.51μs | 6.83μs | 6.65μs |  6.97μs |  6.79μs |    6.70μs |     6.83μs |
| `hashtables` (Linear)               | 6.32μs | 6.79μs | 6.87μs | 6.76μs |  6.78μs |  6.82μs |    7.54μs |     6.85μs |
| `hashtables` (Cuckoo)               | 6.70μs | 6.52μs | 6.82μs | 6.92μs |  6.85μs |  6.94μs |    6.76μs |     6.82μs |
| `HashMap`                           | 6.19μs | 7.17μs | 8.41μs | 9.30μs | 11.2 μs | 16.3 μs |   21.3 μs |    35.5 μs |
| `brokkr-hashtables` (storable keys) | **1.37μs** | **1.68μs** | **1.92μs** | **1.27μs** |  **1.60μs** |  **1.18μs** |    **1.25μs** |     **1.47μs** |
| `vector-hashtables` (storable keys) | 3.67μs | 3.88μs | 4.03μs | 4.42μs |  6.91μs |  6.08μs |    6.38μs |     6.99μs |
| `brokkr-hashtables` (prim     keys) | **1.35μs** | **1.61μs** | **1.89μs** | **1.28μs** |  **1.48μs** |  **1.32μs** |    **1.36μs** |     **1.44μs** |
| `vector-hashtables` (prim     keys) | 3.81μs | 4.05μs | 4.01μs | 4.10μs |  5.42μs |  5.86μs |    5.91μs |     6.39μs |
| `IntMap`                            | 3.10μs | 3.53μs | 4.69μs | 8.18μs | 12.1 μs | 20.7 μs |   48.2 μs |   106   μs |

`Int` keys, using the int value as the hash. **Invalid** lookups. Each run looks up one thousand invalid keys from a table with the given size. Values are always boxed. The load factor for this libraries tables is always `0.75`.

| Name                                |   25   |   50   |   100  |  1_000 |  10_000 | 100_000 | 1_000_000 | 10_000_000 |
|-------------------------------------|--------|--------|--------|--------|---------|---------|-----------|------------|
| `brokkr-hashtables` (boxed    keys) | 5.05μs | 6.88μs | 8.67μs | 3.64μs |  4.83μs |  **2.98μs** |    **3.86μs** |     **4.77μs** |
| `vector-hashtables` (boxed    keys) | **2.93μs** | **2.68μs** | **2.47μs** | **2.71μs** |  **3.91μs** |  4.22μs |    4.71μs |     5.14μs |
| `hashtables` (Basic)                | 6.60μs | 7.22μs | 7.24μs | 7.81μs | 12.50μs |  7.30μs |    6.97μs |    11.0 μs |
| `hashtables` (Linear)               | 6.19μs | 7.12μs | 7.23μs | 7.83μs | 12.30μs |  7.21μs |    7.20μs |    11.2 μs |
| `hashtables` (Cuckoo)               | 6.31μs | 8.46μs | 7.25μs | 7.83μs | 12.40μs |  7.33μs |    7.08μs |    11.0 μs |
| `HashMap`                           | 4.23μs | 4.86μs | 5.35μs | 5.14μs |  5.11μs |  8.22μs |    9.80μs |    12.3 μs |
| `brokkr-hashtables` (storable keys) | **1.52μs** | **1.80μs** | **2.06μs** | **1.32μs** | **1.61μs** |  **1.24μs** |    **1.37μs** |     **1.58μs** |
| `vector-hashtables` (storable keys) | 2.89μs | 2.60μs | 2.46μs | 2.75μs |  3.90μs |  4.01μs |    4.81μs |     5.10μs |
| `brokkr-hashtables` (prim     keys) | **1.67μs** | **1.76μs** | **2.18μs** | **1.38μs** | **1.58μs** |  **1.39μs** |    **1.45μs** |     **1.62μs** |
| `vector-hashtables` (prim     keys) | 2.75μs | 2.63μs | 2.68μs | 2.71μs |  3.89μs |  4.54μs |    4.72μs |     4.87μs |
| `IntMap`                            | 4.04μs | 4.59μs | 5.58μs | 8.05μs | 10.9 μs | 18.4 μs |   44.6 μs |    94.5 μs |

> There is no variation with unboxed values presented because those numbers match their boxed values counterpart. For `lookup` only the key storage matters and `Storable`/`Prim` are far ahead of everything else.

#### Insert + Delete

`Int` keys and values, using the int value as the hash. Each run inserts and deletes 2000 keys, so 4000 total operations. Both operations will always succeed. The table size is kept constant at n. The load factor for this libraries hashtables is `0.75`.

Notation: B/S refers to boxed keys and storable values. P refers to prim.

| Name                              |   25   |    50   |   100  |  1_000 | 10_000 | 100_000 | 1_000_000 | 10_000_000 |
|-----------------------------------|--------|---------|--------|--------|--------|---------|-----------|------------|
| `brokkr-hashtables` (B/B)         | **11.2μs** |  **12.7μs** | **12.1μs** | **11.8μs** | **13.5μs** |  **11.7μs** |    **14.4μs** |     25.5μs |
| `vector-hashtables` (B/B)         | 24.6μs |  24.2μs | 24.2μs | 13.9μs | 14.9μs |  16.0μs |    **14.6μs** |     **15.7μs** |
| `hashtables` (Basic)              | 40.3μs |  45.5μs | 41.5μs | 25.9μs | 31.9μs |  45.4μs |    38.0μs |     49.5μs |
| `hashtables` (Linear)             | 50.8μs |  59.4μs | 59.9μs | 82.5μs | 54.0μs |  81.0μs |    133μs  |      586μs |
| `hashtables` (Cuckoo)             | 88.9μs |  88.1μs | 90.9μs | 77.5μs | 79.7μs |  87.6μs |    95.9μs |      106μs |
| `HashMap`                         | 139μs  | 170μs   | 212μs  | 289μs  | 396μs  |  655μs  |    968μs  |     1.10ms |
| `brokkr-hashtables` (B/S)         | **13.5μs** |  **15.2μs** | **14.5μs** | 12.3μs | 14.5μs |  **11.6μs** |    **12.4μs** |     **14.9μs** |
| `vector-hashtables` (B/S)         | 23.9μs |  22.6μs | 22.5μs | **11.7μs** | **12.1μs** |  13.0μs |    **12.6μs** |     15.6μs |
| `brokkr-hashtables` (B/P)         | **14.4μs** |  **15.0μs** | **14.5μs** | 12.5μs | 14.3μs |  **11.6μs** |    **13.0μs** |     16.1μs |
| `vector-hashtables` (B/P)         | 26.1μs |  23.4μs | 23.2μs | **11.9μs** | **12.3μs** |  13.8μs |    **13.0μs** |     **14.6μs** |
| `brokkr-hashtables` (S/B)         |  **9.5μs** |  **10.0μs** |  **9.6μs** |  **7.9μs** |  **8.7μs** |   **8.9μs** |     **9.9μs** |     **10.2μs** |
| `vector-hashtables` (S/B)         | 25.6μs |  24.6μs | 26.9μs | 13.9μs | 15.3μs |  16.1μs |    14.9μs |     17.8μs |
| `brokkr-hashtables` (S/S)         |  **7.5μs** |   **8.5μs** |  **8.0μs** |  **6.8μs** |  **7.5μs** |   **6.3μs** |     **6.6μs** |      **6.9μs** |
| `vector-hashtables` (S/S)         | 20.4μs |  19.4μs | 18.1μs | 12.0μs | 12.3μs |  13.2μs |    12.8μs |     14.5μs |
| `brokkr-hashtables` (S/P)         |  **8.6μs** |  **9.25μs** |  **9.0μs** |  **7.8μs** |  **8.2μs** |   **7.4μs** |     **7.5μs** |      **7.7μs** |
| `vector-hashtables` (S/P)         | 19.5μs |  19.6μs | 18.5μs | 12.8μs | 12.7μs |  14.1μs |    12.8μs |     14.9μs |
| `brokkr-hashtables` (P/B)         |  **8.7μs** |  **9.64μs** |  **9.3μs** |  **8.1μs** |  **9.1μs** |   **9.7μs** |    **10.5μs** |     **11.0μs** |
| `vector-hashtables` (P/B)         | 27.0μs |  24.8μs | 24.6μs | 14.3μs | 15.0μs |  16.0μs |    15.1μs |     16.4μs |
| `IntMap`                          | 106μs  | 140μs   | 169μs  | 308μs  | 516μs  | 751μs   |    1.07ms |     1.27ms |
| `brokkr-hashtables` (P/S)         | **10.7μs** |  **11.3μs** | **11.0μs** |  **9.7μs** | **10.2μs** |   **9.6μs** |     **9.7μs** |      **9.6μs** |
| `vector-hashtables` (P/S)         | 21.0μs |  20.2μs | 19.4μs | 11.5μs | 12.9μs |  13.9μs |    12.3μs |     13.6μs |
| `brokkr-hashtables` (P/P)         | **10.6μs** |  **11.3μs** | **10.9μs** |  **9.6μs** | **10.2μs** |   **9.5μs** |     **9.6μs** |      **9.7μs** |
| `vector-hashtables` (P/P)         | 21.0μs |  20.8μs | 20.0μs | 11.9μs | 16.8μs |  13.2μs |    12.9μs |     14.5μs |

#### Bulk inserts

Benchmarks resize operations more than it represents inserts.

No data just yet. `tasty-bench` and `criterion` have very different results here, so I am not yet sure what to do with this benchmark. But `brokkr-hashtables` is fast with `reserve` or on unboxed storage. For very large tables and boxed storage `vector-hashtables` performs better on bulk inserts. `hashtables` and the immutable containers always loose this benchmark.

### Conclusion

The following segments look at `hashtables`/`vector-hashtables`/`IntMap`/`HashMap` and outlines some conclusions from the benchmark data and some other facts about these libraries.

Statements such as "slow/fast" or "more/less overhead" are made relative to this library.

#### hashtables

Pro:

- Variety of different table implementations
- Prime number based growth. Grows slower than powers of 2.[^4]

Con:

- Slow lookups
- Slow insert and delete
- Slow table resizes
- Slightly more overhead per key (ignoring `Storable` tables and their padding)
- Some tables degrade when too many elements are deleted
- No option of unboxed/flat storage. This means more gc pressure, more allocations and more memory overhead.
- No options to manipulate the seed of the hash function or the hash function itself. `hashable` is used.
- No options to tweak the max load factor
- No option to `reserve` space beyond the initial creation size

`hashtables` is disappointing. It is way slower than both `brokkr-hashtables` and `vector-hashtables` and for small sizes doesn't even manage to reliably beat `HashMap` and `IntMap`.

#### vector-hashtables

Pro:

- Cheaper resizes lead to faster bulk inserts (ignoring `reserve` that is)
- Immutable api
- Allows unboxed, not just primitive data
- Fast failing lookups for `Boxed` components on small to medium-sized tables, because it stores hashes and compares against those first, providing a cheaper inequality check. This advantage vanishes when keys are `Storable` or `Prim` and have a cheap comparison function. And even for `Boxed` this effect vanishes for larger tables as their lookup performance degrades for both successful and failing lookups. [^3]
- Prime number based growth. Grows slower than powers of 2.[^4]

Con:

- Slow lookups
- Slow insert and delete for all tables for less than a thousand elements. Afterwards the boxed key versions are on par.
- Higher overhead per key value pair: 3 word sized integers per key value pair in the table.
- No options to manipulate the seed of the hash function or the hash function itself. `hashable` is used.
- No options to tweak the max load factor
- No option to `reserve` space beyond the initial creation size

`vector-hashtables` is a solid choice for storing boxed elements and has good resize performance, but lacks the options that `brokkr-hashtables` has. But `brokkr-hashtables` is on par or usually faster even for boxed keys, and when it comes to unboxed data `vector-hashtables` is left far behind.

[^3]: If I had to guess, I'd blame their collision strategy. It's effectively a linked list per bucket, but the list is encoded as a flat array and shared among all buckets. Clever, as long as it fits cache and entries stay close, but a nightmare of memory loads for larger tables.

[^4]: While they both have a predefined list of primes, both libraries miss out on optimizing the remainder by constant operation performed on every lookup/insert/delete.

#### IntMap/HashMap

> Comparing an immutable variant to a mutable hashtable may not seem fair, and it isn't. But for small sizes, they are actually quite competitive. Slower than `brokkr-hashtables` still, but close to the `Boxed` key versions.

Pro:

- Immutable api
- Structural sharing

Cons:

- Slow lookups
- Slow inserts
- `IntMap` stores only boxed haskell values
- `HashMap` stores both keys and values boxed
- No options to manipulate the seed of the hash function or the hash function itself. `hashable` is used.

As expected, the two cannot really compete. However, for being immutable containers with structural sharing, they do get quite close at small sizes.

## Credits

ska:flat_hash_map: https://github.com/skarupke/flat_hash_map

This library is part of the ecosystem supporting `Brokkr` a Minecraft server written in Haskell.
