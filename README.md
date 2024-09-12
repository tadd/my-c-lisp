My C Lisp
=========

A study or an étude for my daily hacking exercise, a Scheme engine that aims[^1] for
[R<sup>5</sup>RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/)
compliance.

You'll find nothing unique or interesting here; this is an interpreter with a plain and
old C implementation (C17 with some extensions).

You can use this under the [MIT license](./LICENSE.md).

[^1]: just a little[^2] bit
[^2]: really

## TODO

* Implement syntax/procedures marked with `//-`
* Refactor `libtest.scm` with `define-syntax`
* Proper tail recursion
* Better parsing based on formal syntax
* Report error location with `line:column` and function names in a call stack
