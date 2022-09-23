
# Interpreter-rs

[![Documentation](https://docs.rs/dacx0501/badge.svg)](https://docs.rs/dacx0501)

A custom hand-written interpreter from scratch

## What is Interpreter-rs

This project is an interpreter for an expression heavy dynamically typed language with rust-like syntax. Its purpose is to teach the building of an interpreter

## What is supported

There is a lexer and a parser that are _finished_. This supports much of what a typical language would consist of although it does currently lack loops. The parser produces and AST that is passed to the evaluator. This does include support for proper parsing of operator precedence using a recursive descent parser. The evaluator is in nearly finished and supports expressions, variable binding, a REPL and has high quality error messages.

## A few short examples of code that can be properly parsed and evaluated

```rust
let x = 5;
foobar;
let y = (3 + 3) * 5;
if a {
  b
} else if c {
  d
} else {
  e
}
let multiply = fn(x, y){x * y};
100 / 20
```
