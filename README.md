# rustc-jvm
A very much incomplete elementary Rust compiler that targets the JVM. Supports C-like subset of Rust. Written quickly for a course project and currently unmaintained.

This project contains a lot of unused code, repeated code, and not particularly idiomatic Rust. It was written a year ago for a course project that needed to be done quickly, and at that time I was still getting more familiar with Rust. This code is just for reference and is by no means a good example of a compiler system. It is just a proof of concept for the idea of using Rust to compile a Rust subset to JVM bytecode. This was based on the idea that we could use the Java [Unsafe API](https://hg.openjdk.java.net/jdk/jdk/file/tip/src/jdk.unsupported/share/classes/sun/misc/Unsafe.java) to write Rust code that can run on the JVM without using the garbage collector by leveraging Rust's static memory guarantees to allocate and free memory as normal but through the JVM's API to access memory not managed by the GC. This goal, however, was not reached, as I currently think that the method taken here to rewrite an entire Rust compiler for the job is not the best way. I think an extension of the official `rustc` or the WebAssembly road would be better avenues to achieve this goal. The lexer is largely borrowed from `rustc` in addition to many types for the AST, but everything else was written by hand.

If you are interested in seeing what the compiler can do, you can try compiling and running the code in `./test/test`:

```
cargo run -- -c InsertionSort -i ./test/test/src/main.rs
java -noverify InsertionSort 
```
