# Trefoil: A Functional Programming Language in OCaml

## Overview
Trefoil is a functional programming language designed and implemented in OCaml. Developed incrementally over three assignments, it evolves from a simple arithmetic and list-processing language to a feature-rich environment with user-defined functions, struct-based data modeling, and pattern matching.

## Features
- **Arithmetic Operations**: Addition, subtraction, multiplication, and equality comparison.
- **Conditional Evaluation**: `if` expressions and multi-way `cond` expressions.
- **Lists & Pairs**: `nil`, `cons`, `car`, `cdr`, `nil?`, and `cons?` operations.
- **Function Definitions & Calls**: First-class functions and lexical scoping.
- **Pattern Matching**: `match` expressions for destructuring data.
- **Structs**: User-defined struct types with constructors and field accessors.
- **Lambda Expressions**: Anonymous functions with closures.
- **Printing & Symbols**: Built-in support for symbol literals and print expressions.

## Evolution of Trefoil
### **Trefoil v2 (Homework 5)**
- Introduces basic arithmetic (`+`, `-`, `*`, `=`).
- Supports conditionals (`if`), lists (`cons`, `car`, `cdr`), and tests.
- Implemented as an interpreter over a dynamically scoped environment.

### **Trefoil v3 (Homework 6)**
- Adds multi-variable `let` expressions.
- Introduces function definitions and function calls.
- Implements `cond` for multi-way branching.

### **Trefoil v4 (Homework 7)**
- Introduces structs with user-defined field access.
- Supports pattern matching (`match`) with wildcard and structured patterns.
- Implements first-class functions with lambda expressions and closures.

## Tech Stack
- **Language**: OCaml
- **Parsing**: PST (Parenthesized Symbol Trees, similar to S-expressions)
- **Evaluation**: Recursive tree-walking interpreter
- **Testing**: OCaml test framework
