# Native Haskell implementation of Avro

This is a Haskell [Avro](https://avro.apache.org/) library useful for decoding
and encoding Avro data structures.  Avro can be thought of as a serialization
format and RPC specification which induces three separable tasks:

* *Deserialization* - This library has been used "in anger" for deserialization
  of avro container files and should suffice for general needs.
* *Serialization* - Encoding routines exist and have seen some use, but much
  less than the deserialization path.  Finding a good serialization API is
  harder in general because it involves zipping up two otherwise unrelated
  structures (the actual ADT and the Avro Schema).
* *RPC* - There is currently no support for Avro RPC in this library.

Please see the [TODO](TODO)
