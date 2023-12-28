# ShortLang
A programming language designed for code golfing

# ShortLang Documentation

Welcome to the official documentation for ShortLang, a fast and concise interpreted programming language designed for code golfing. ShortLang is implemented in Rust and boasts a performance that is 5 times faster than Python.

## Table of Contents
1. [Basic Data Types](#basic-data-types)
2. [Hello World](#hello-world)
3. [Operators](#operators)
4. [Variables](#variables)
5. [Arrays](#arrays)
6. [Functions](#functions)
    - [Inline Function](#inline-function)
    - [Multiline Function](#multiline-function)
7. [Comments](#comments)
8. [Conditional Statements](#conditional-statements)
9. [Loops](#loops)

## Basic Data Types

- `int`
- `float`
- `bool`
- `array`
- `nil`

## Hello World

To print "Hello World" to the standard output with a newline, use:

```
$"Hello World"
```

If you want to print without a newline, use:

```
$$"Hello World"
```

ShortLang provides `print` and `println` functions in the standard library for the same purpose.

<br>

For the sake of readability, we'll use `print` and `println` in the examples.

## Operators

| Operator | Description              |
|----------|------------------------  |
| `+`      | Addition                 |
| `-`      | Subtraction              |
| `*`      | Multiplication           |
| `/`      | Division                 |
| `%`      | Modulus                  |
| `++`     | Increment                |
| `--`     | Decrement                |
| `**`     | Exponent                 |
| `<`      | Less than                |
| `>`      | Greater than             |
| `==`     | Equal than               |
| `<=`     | Less than or equal to    |
| `>=`     | Greater than or equal to |
| `&&`     | And                      |
| `||`     | Or                       |

## Variables

Variables can be created without a semicolon. For example:

```
var_name = value
```

```
number = 42
$("The number is: " + number)
```

## Arrays

The following code shows an example array

```
fibonacci = [0, 1, 1, 2, 3, 5, 8, 13]

$("5th fibonacci number is " + fibonacci[5])
```

## Functions

### Inline Function

Similar to lambda function in python

```
add a b: a + b
println(add(10, 20)) // prints 30
```

### Multiline Function

```
add a b: {
  println("You are adding: " + a + " and " + b)
  a + b
}

println(add(10, 20))
```

The last expression in a multiline function is the return value.

<br>

You can also use `&` explicitly to denote the return value.
```
max a b: {
    a > b ? 
} 
```

## Comments

Comments start with `//`.

## Conditional Statements

Ternary operator for if-else:

```
a = 1
a == 1 ? println("The expression is true") : println("The expression is false")
```

Block execution:

```
true ? {
    // code here will be executed
} : {
   // if condition is false
}
```

The else block can be removed

```
a == 10 ? println("a is equal to 10")
```

## Loops

ShortLang currently supports a `while` loop:

```
>. condition {
    // code to be executed while the condition is true
}
```

```
i = 0
>. i < 10 {
    $("Value of i is: " + i)
    i ++
}
```

This code prints:

```
Value of i is: 0
Value of i is: 1
Value of i is: 2
Value of i is: 3
Value of i is: 4
Value of i is: 5
Value of i is: 6
Value of i is: 7
Value of i is: 8
Value of i is: 9
```

## Misc
### Factorial function example
```
factorial x: x < 2 ? 1 : x * factorial(x - 1)
$factorial(10)
```

Prints

```
265252859812191058636308480000000
```