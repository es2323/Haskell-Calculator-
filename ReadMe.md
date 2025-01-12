Haskell Expression Calculator

Overview

This Haskell-based calculator is designed to evaluate mathematical expressions by parsing and constructing a binary tree (Abstract Syntax Tree) representation of the input. The tree is then evaluated to produce the result. The calculator supports various operations, including basic arithmetic, exponentiation, modulus, and operations involving parentheses.

Features

Supported Operations

Addition (+): Performs addition of two numbers.

Example: 2 + 3 → 5.0

Subtraction (-): Performs subtraction of one number from another.

Example: 5 - 2 → 3.0

Multiplication (*): Multiplies two numbers.

Example: 4 * 3 → 12.0

Division (/): Divides one number by another, ensuring no division by zero occurs.

Example: 8 / 2 → 4.0

Exponentiation (^): Computes the power of a base number raised to an exponent.

Example: 3^2 → 9.0

Modulus (%): Computes the remainder of division.

Example: 10 % 3 → 1.0

Parentheses: Allows grouping of operations to enforce precedence.

Example: (2 + 3) * 4 → 20.0

Real Numbers

Precision: The calculator supports real numbers and outputs results rounded to one decimal place for clarity.

Example: 5.1 * 4.2 → 21.4

Example: (10.5 + 2.3) * 4 → 51.2

Error Handling

Invalid Input: The calculator identifies and reports errors in expressions, such as:

Mismatched parentheses

Unsupported characters

Safe Division: Ensures division by zero is avoided and reports an appropriate error message.

Modularity

The implementation uses recursive functions to parse and evaluate expressions, making the code extensible for future operations (e.g., trigonometric functions, logarithms).

Usage

Input Format

Expressions should be provided as a string.

Supported operators: +, -, *, /, ^, %.

Parentheses can be used for grouping.