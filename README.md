
# Interpreter-rs

[![Documentation](https://docs.rs/dacx0501/badge.svg)](https://docs.rs/dacx0501)

A custom hand-written interpreter from scratch

## What is Interpreter-rs

This project is an interpreter for an expression heavy dynamically typed language with rust-like syntax. Its purpose is to teach the building of an interpreter

## What is supported

There is a lexer and a parser that are _finished_. This supports much of what a typical language would consist of although it does currently lack loops. The parser produces and AST that is ready to be interpreted. This does include support for proper parsing of operator precedence using a recursive descent parser

## What still needs to be implemented

The evaluation of the AST needs to be completed and printed into a REPL while giving good support for the great error messages that the parser currently produces

## A few short examples of code that can be properly parsed

```rust
let x = 5;
foobar;
let y = (3 + 3) * 5;
let multiply = fn(x, y){x * y};
```
