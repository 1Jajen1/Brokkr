# Brokkr

[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

*Active supported Minecraft version: `1.19.3`*

The Brokkr project contains a Haskell implementation of a Minecraft server and several libraries to work with Minecrafts data from the protocol to the world files.

Its goals are to provide both usable libraries to develop Minecraft related applications and a close to vanilla game server.

## Libraries

List of all libraries currently living in the repository. The status has the following meaning:

- Stable: The library is not expected to change much, and a version has been uploaded to hackage
- Experimental: The library is close to feature complete, but may still be changed significantly during development (Useable but unstable).
- Work in progress: The library is worked on, use outside Brokkr is not recommended as it is missing features and may change significantly.
- Concept: Hardly any work has been put in. The library merely exists to explore some idea, but it may someday develop into an actual library.


| Name                 | Purpose                                                               | Status                   |
|----------------------|-----------------------------------------------------------------------|--------------------------|
| brokkr-varint        | Utilities for dealing with Minecrafts variable-length number encoding | Experimental             |
| brokkr-nbt           | Utilities for dealing with NBT data                                   | Experimental             |
| brokkr-anvil         | Utilities for loading chunk data from anvil files                     | Experimental             | 
| brokkr-packed-vector | Datatype for variable sized packed vectors                            | Experimental             |
| brokkr-hashtables    | Fast flexible hashtable                                               | Experimental             |
| brokkr-block         | Template-haskell generated block states                               | Work in progress         |
| brokkr-hecs          | Archetype entity component system based on flecs                      | Work in progress         |
| brokkr-packet        | Datatypes and serialization for the Minecraft protocol                | Work in progress         |
| brokkr-registry      | Datatypes which encode various parts of Minecraft registries          | Work in progress         |
| brokkr-concurrent    | Concurrency datastructures and classes                                | Concept                  |
| brokkr-zlib-ng       | zlib-ng bindings                                                      | Concept                  |

For more information on each library and a detailed status report visit their individual readmes. TODO Link them with the name

## The server

The server itself is currently in a joinable status. Chunkloading and basic movement has been implemented. The current area of work is around designing the entity systems, which should then lead to at the very least clients being able to see each other.

For closer details on how to run the server and how it is designed visit its readme.
