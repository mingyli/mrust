# mRust

A compiler for a subset of Rust targeting LLVM IR.

## Features

Notable features include:

[ ] RAII + move semantics
[ ] borrow checker
[ ] mutability checker
[ ] sum types / tagged unions
[ ] pattern matching

## Setup

```sh
$ brew install llvm
$ LLVM_SYS_110_PREFIX=/opt/homebrew/opt/llvm cargo run < input/example.rs
$ /opt/homebrew/opt/llvm/bin/lli mrust.ll
```

## Related Reading

<http://llvm.org/docs/GettingStarted.html#an-example-using-the-llvm-tool-chain>

<https://crates.io/crates/llvm-sys#llvm-compatibility>

