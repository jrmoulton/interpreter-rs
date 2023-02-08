
# Interpreter-rs

[![Documentation](https://docs.rs/dacx0501/badge.svg)](https://docs.rs/dacx0501)

A custom hand-written interpreter ***and*** compiler from scratch (based on "Building an interpreter in Go" by Thorsten Ball)

## What is Interpreter-rs

This project is an interpreter and a compiler (these are separate language implementations that share the same parser) for an expression heavy dynamically typed language with python/rust like syntax. 

## What is supported

This language supports expressions, functions, assignment, recursion, closures as well as a few language built-ins. The interpreter/evaluator has a more complete implementation than the compiler currently. 

## Examples

### Fibbonacci Example
```rust
let fib = fn(n) {
   if n < 2 {
       n
   } else {
       fib(n-1) + fib(n-2)
   }
};
fib(25)
```

### Closure Example
``` rust
let new_adder = fn(x) { 
  fn(y) {x + y} // Implicit return of a function that adds y to the x (this is the magic of the closure)
}; 
let add_two = new_adder(2);
add_two(3) // -> returns 5
```
