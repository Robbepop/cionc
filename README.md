# Cionc - The Cion Compiler

|        Linux        |        Windows        |       Codecov        |       Coveralls        |
|:-------------------:|:---------------------:|:--------------------:|:----------------------:|
| [![travis][10]][11] | [![appveyor][20]][21] | [![codecov][30]][31] | [![coveralls][40]][41] |

Cion is my toy programming language and this is the compiler for it.

## Planned Features

 - [x] Lexer
 - [x] AST Definition
 - [ ] Error Reporting
 - [ ] Parser
 - [ ] AST Pretty Printer
 - [ ] Semantic Analysis
 - [ ] LLVM-IR Codegen
 - [ ] CLI & Driver

## Primitive Types

|    Cion          |    Rust          | Description |
|:----------------:|:----------------:|:------------|
| ()               | ()               | The empty (or void) type. |
| num              | -                | A real number type that is space optimized for bit-widths less than or equal to 64 bits but that can handle any bit-width. |
| bool             | bool             | The truth type. |
| (A,B,...)        | (A,B,...)        | The generic tuple type. |
| [T;N]            | [T;N]            | The fixed-size array type. |
| [T]              | [T]              | The unsized-array type. |
| fn(A,B,...) -> C | fn(A,B,...) -> C | Functions and closures. |
| &T               | &T               | Shared-Reference to T. |
| &mut T           | &mut T           | Mutable-Reference to T. |
| str              | str              | Unsized UTF-8 character sequence. |

## Example Code

Simple "Hello, World!" programm.

```
main := fn()
    println("Hello, World!")
```

Recursive maths faculty function.

```
faculty := fn(n: num) -> num
    match n
        | 0 => 1
        | _ => n * faculty_recursive(n - 1)
```

Struct and custom type definition.

```
struct Age(num)

struct Person
	name    : String
	age     : Age
	children: Vec<Person>
```

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Dual licence: [![badge][80]](LICENSE-MIT) [![badge][81]](LICENSE-APACHE)

[10]: https://travis-ci.org/Robbepop/cionc.svg?branch=master
[11]: https://travis-ci.org/Robbepop/cionc

[20]: https://ci.appveyor.com/api/projects/status/ebvd9x57t7cc050v/branch/master?svg=true
[21]: https://ci.appveyor.com/project/Robbepop/cionc/branch/master

[30]: https://codecov.io/gh/Robbepop/cionc/branch/master/graph/badge.svg
[31]: https://codecov.io/gh/Robbepop/cionc/branch/master

[40]: https://coveralls.io/repos/github/Robbepop/cionc/badge.svg?branch=master
[41]: https://coveralls.io/github/Robbepop/cionc?branch=master

[80]: https://img.shields.io/badge/license-MIT-blue.svg
[81]: https://img.shields.io/badge/license-APACHE-orange.svg
