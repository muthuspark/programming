+++
title = "Beginner's Guide to Standard ML"
date = 2024-12-18
toc = true
readTime = true
+++

## Introduction to Standard ML

### What is Standard ML?

Standard ML (SML) is a general-purpose, statically-typed programming language with a strong emphasis on functional programming.  It's known for its rigorous type system, which helps catch errors early in the development process, and its powerful module system, which promotes code reusability and organization.  SML's design prioritizes expressiveness and mathematical precision, making it suitable for tasks requiring correctness and clarity, such as compiler construction, formal verification, and program analysis. Unlike many modern languages, SML features an interactive top-level environment, allowing for immediate experimentation and testing of code. This interactive nature makes it an excellent choice for learning functional programming concepts.


### Why Learn Standard ML?

Learning Standard ML offers several benefits:

* **Strong foundation in functional programming:** SML provides a solid grounding in functional programming paradigms, which are increasingly relevant in modern software development. Understanding functional concepts improves your ability to write cleaner, more maintainable, and often more efficient code in other languages.

* **Improved programming skills:** SML's type system forces you to think carefully about data structures and their relationships, leading to improved coding practices in general. The focus on immutability reduces the likelihood of common programming errors stemming from mutable state.

* **Enhanced understanding of compiler design:** SML's design and features are closely tied to compiler design principles. Learning SML can provide valuable insights into how compilers work.

* **Development of formal reasoning skills:** SML's mathematical rigor encourages a more formal approach to problem-solving, improving your ability to reason about and verify the correctness of your programs.


### Setting up your environment

To begin programming in Standard ML, you'll need an SML implementation.  Several are available, including:

* **MLton:** A highly optimizing compiler known for its speed and efficiency.
* **Poly/ML:**  A mature and widely used implementation with a rich set of libraries.
* **SML/NJ:** Another popular implementation with good support and documentation.

The specific installation process varies depending on your operating system and chosen implementation.  Consult the documentation for your selected implementation for detailed instructions. Generally, this involves downloading a suitable package for your system and following the installation wizard or instructions provided.  After installation, you should be able to start the SML interpreter by typing the appropriate command in your terminal (e.g., `mlton` or `poly`).


### First SML program: Hello World!

The classic "Hello, World!" program in SML is straightforward:

```sml
fun main () = print ("Hello, World!\n");
```

This code defines a function `main` that takes no arguments and prints the string "Hello, World!" to the console, followed by a newline character (`\n`). To run this program:

1. **Save the code:** Save the code in a file named (for example) `hello.sml`.
2. **Compile (if necessary):**  Some implementations (like MLton) require compilation.  The specific command depends on your chosen implementation;  refer to the implementation's documentation.  For example, with MLton it might look like:  `mlton hello.sml`.
3. **Run:**  If compilation is necessary the compiled file can then be executed.  Alternatively, if using an interpreter such as Poly/ML, simply load the file into the REPL:  `use "hello.sml";` followed by `main();`.  This will execute the `main` function and print the output.


This simple example demonstrates the basic structure of an SML program and provides a starting point for your exploration of the language. Remember to consult the documentation for your chosen SML implementation for more details and advanced features.


## Basic Syntax and Data Types

### Values and Types

Standard ML is a statically-typed language, meaning that every value has an associated type, and the type checker ensures that operations are performed on compatible types. This prevents many common programming errors at compile time.  Types are inferred automatically in most cases, relieving the programmer from explicitly annotating every expression with its type.  Values are the fundamental building blocks of SML programs.  They can be literals (like numbers or strings), variables referencing values, or results of expressions.  The type of a value determines the operations that can be performed on it.

### Integers, Reals, Booleans, Strings

SML provides several built-in data types:

* **Integers (`int`):** Represent whole numbers (e.g., `1`, `-10`, `0`, `42`).  Standard arithmetic operations (+, -, *, /, div, mod) are available.  `div` performs integer division, and `mod` gives the remainder.

* **Reals (`real`):** Represent floating-point numbers (e.g., `3.14`, `-2.5`, `0.0`).  Standard arithmetic operations (+, -, *, /) are supported.

* **Booleans (`bool`):** Represent truth values (`true` and `false`).  Boolean operations include `andalso` (logical AND), `orelse` (logical OR), and `not` (logical NOT). Note that `andalso` and `orelse` are short-circuiting operators.

* **Strings (`string`):** Represent sequences of characters (e.g., `"Hello"`, `"World!"`).  String concatenation is done using the `^` operator (e.g., `"Hello" ^ " World!"`).


### Type inference

SML's type inference system automatically determines the type of expressions. This avoids the need for explicit type declarations in many cases, making the code cleaner and more concise. The compiler infers the types based on how values are used in expressions. For example:

```sml
val x = 10; (* x is inferred to be of type int *)
val y = 3.14; (* y is inferred to be of type real *)
val z = x + 5; (* z is inferred to be of type int *)
```

The compiler will report a type error if an operation is performed on incompatible types (e.g., adding an integer and a string).

### Variables and Constants

Variables are used to store values. In SML, variables are immutable; once a value is assigned to a variable, it cannot be changed.  This promotes functional programming style and helps avoid many side-effect related bugs.  Variables are declared using the `val` keyword:

```sml
val x = 10;
val name = "Alice";
```

Constants are similar to variables but often used to represent values that should not be changed during the program's execution.  There's no specific keyword for constants in SML; the immutability of variables effectively serves as a form of constant declaration if the intent is to not change the value after it's bound to a variable.


### Operators and Expressions

Operators perform operations on values.  SML supports a variety of operators, including arithmetic operators (+, -, *, /, div, mod), relational operators (=, <>, <, >, <=, >=), logical operators (andalso, orelse, not), and string concatenation (^).

Expressions are combinations of values, variables, operators, and function calls.  They evaluate to a value. Examples include:

```sml
1 + 2 (* arithmetic expression *)
x > 5 (* relational expression *)
"Hello" ^ " " ^ "World!" (* string concatenation *)
```

The order of operations follows standard mathematical precedence rules. Parentheses can be used to override precedence.  Function application is done by simply writing the function name followed by the arguments in parentheses.


## Functions and Control Structures

### Defining functions

Functions in SML are first-class citizens; they can be passed as arguments to other functions, returned as results, and stored in data structures. Functions are defined using the `fun` keyword, followed by the function name, parameter list, and function body.

```sml
fun add (x: int, y: int): int = x + y;
fun square (x: real): real = x * x;
fun greet (name: string): string = "Hello, " ^ name ^ "!";
```

These examples show functions with type annotations.  While type annotations are optional in many cases (due to type inference), they are highly recommended for clarity and to help catch errors early.


### Function application

Function application is straightforward: simply write the function name followed by its arguments enclosed in parentheses.

```sml
val sum = add(5, 3); (* sum will be 8 *)
val area = square(5.0); (* area will be 25.0 *)
val message = greet("Alice"); (* message will be "Hello, Alice!" *)
```

### Conditional statements (if-then-else)

Conditional statements use the `if-then-else` construct.

```sml
fun abs (x: int): int =
  if x >= 0 then x else ~x; (* ~x is the negation of x *)

fun max (x: int, y: int): int =
  if x > y then x else y;
```

The `if` expression evaluates a boolean condition. If the condition is `true`, the `then` branch is executed; otherwise, the `else` branch is executed.


### Pattern matching

Pattern matching is a powerful feature of SML that allows you to select different code branches based on the structure of data. It provides a concise and elegant way to handle different cases within a function.

```sml
fun factorial 0 = 1
  | factorial n = n * factorial(n - 1);

fun isEven n =
    case n mod 2 of
        0 => true
      | 1 => false
      | _ => false; (* handles any other case, though it should never be reached here *)

```

The `factorial` function uses pattern matching on the input `n`. If `n` is 0, it returns 1; otherwise, it recursively computes `n * factorial(n-1)`. The `isEven` function showcases pattern matching within a `case` expression.


### Loops and Recursion

SML doesn't have explicit loop constructs like `for` or `while` loops found in imperative languages. Iteration is typically achieved through recursion.  Recursion is a powerful technique where a function calls itself.

```sml
fun sumList [] = 0
  | sumList (head::tail) = head + sumList tail;

fun power (x: real, n: int): real =
  if n = 0 then 1.0
  else if n > 0 then x * power(x, n - 1)
  else 1.0 / power(x, ~n);  (* handles negative exponents *)
```

The `sumList` function recursively sums the elements of a list.  The `power` function calculates x raised to the power of n using recursion, handling both positive and negative exponents. Recursion is a fundamental concept in functional programming and provides an elegant way to express iterative processes.  However, it's crucial to ensure that recursive functions have a well-defined base case to avoid infinite recursion.


## Data Structures

### Tuples

Tuples are finite ordered sequences of values, where each value can have a different type.  They are created by enclosing the values within parentheses, separated by commas.  Tuples are immutable; once created, their elements cannot be changed.

```sml
val point = (10, 20); (* A tuple of two integers *)
val person = ("Alice", 30, true); (* A tuple of a string, integer, and boolean *)
```

To access the elements of a tuple, you can use pattern matching:

```sml
fun distance ((x1: int, y1: int), (x2: int, y2: int)): real =
  Math.sqrt(Real.fromInt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1)));
```

Here, the `distance` function uses pattern matching to directly access the x and y coordinates of two points.

### Lists

Lists are ordered sequences of values of the *same* type.  Unlike tuples, lists are mutable, meaning that you can add or remove elements. Lists are constructed using the `::` (cons) operator which adds an element to the front of the list or using list notation `[e1, e2, e3, ...]`. The empty list is denoted by `[]`.

```sml
val numbers = 1 :: 2 :: 3 :: []; (* List with elements 1, 2, 3 *)
val numbers2 = [1, 2, 3]; (* equivalent list notation *)
val strings = ["hello", "world"];
```

List processing is often done recursively:

```sml
fun sumList [] = 0
  | sumList (head::tail) = head + sumList tail;
```

This `sumList` function recursively sums the elements of a list.  Other common list functions include `List.map`, `List.filter`, and `List.foldl`.  Refer to the SML Basis Library documentation for details on these functions.


### Records

Records are collections of labeled values, where each value has an associated name (label).  They are useful for representing structured data where the elements have meaningful names.

```sml
val person = {name = "Bob", age = 25, city = "New York"};
```

Record fields are accessed using the `.` operator:

```sml
val name = person.name; (* name will be "Bob" *)
val age = person.age;  (* age will be 25 *)
```

Records can also be pattern-matched:


```sml
fun greet ({name, age}: {name: string, age: int}) =
  "Hello, " ^ name ^ "! You are " ^ Int.toString age ^ " years old.";

```

This `greet` function uses pattern matching to extract the `name` and `age` fields from the `person` record.  Records provide a structured way to organize data with named fields, enhancing code readability and maintainability.


## Modules and Structures

SML's module system promotes code organization, reusability, and abstraction.  Modules encapsulate related data types, functions, and other declarations, providing a way to structure larger programs.  The core components of SML's module system are structures and signatures.

### Creating modules

A structure groups together related declarations, similar to a namespace or class in other languages.  Structures are created using the `struct...end` construct.

```sml
structure MyMath = struct
  fun square (x: real) = x * x;
  fun cube (x: real) = x * x * x;
end;
```

This creates a structure named `MyMath` containing functions `square` and `cube`.  To access these functions, you use the `.` operator:

```sml
val x = MyMath.square(5.0);
val y = MyMath.cube(2.0);
```

### Signatures

A signature specifies the interface of a structure. It declares the types and values that are accessible from the outside.  Signatures are defined using the `sig...end` construct.

```sml
signature MATH_SIG = sig
  val square : real -> real;
  val cube : real -> real;
end;
```

This signature `MATH_SIG` specifies that any structure implementing it must provide functions `square` and `cube` with the given types.  You can then check whether a structure matches a signature:

```sml
structure MyMath : MATH_SIG = struct (* ... (implementation as before) ... *) end;
```

This declares `MyMath` to adhere to the `MATH_SIG` signature. The compiler will verify that the structure's interface matches the signature. Signatures enforce modularity and help prevent accidental changes to the interface.

### Functors

Functors are functions that take structures as input and return structures as output. They are a powerful mechanism for code reuse and generating customized modules.

```sml
functor MakeStack (structure Elem: sig type t end) : sig
    type t;
    val empty : t;
    val push : Elem.t * t -> t;
    val pop : t -> Elem.t * t;
    val isEmpty : t -> bool;
  end =
struct
  type t = Elem.t list;
  val empty = [];
  fun push (x, s) = x :: s;
  fun pop [] = raise Empty
    | pop (x::xs) = (x, xs);
  fun isEmpty s = s = [];
  exception Empty;
end;

structure IntStack = MakeStack(struct type t = int end);
structure StringStack = MakeStack(struct type t = string end);

val intStack = IntStack.push(1, IntStack.empty);
val stringStack = StringStack.push("hello", StringStack.empty);

```

This `MakeStack` functor creates a generic stack module. The type of element in the stack is determined by the input structure `Elem`.  `MakeStack` is then used to create specialized stack modules for integers (`IntStack`) and strings (`StringStack`).  Functors allow you to write generic code that can be adapted to different data types, improving code reusability and reducing redundancy.


## Advanced Topics (Optional)

### Exception Handling

Exceptions allow you to handle runtime errors gracefully.  In SML, exceptions are raised using the `raise` keyword and caught using the `handle` clause.

```sml
exception MyError of string;

fun safeDiv (x: int, y: int): int =
  if y = 0 then raise MyError "Division by zero"
  else x div y;

val result =
  (safeDiv (10, 2)) handle MyError msg => (print ("Error: " ^ msg ^ "\n"); 0);
```

The `safeDiv` function raises a `MyError` exception if the divisor is zero. The `handle` clause catches this exception, prints an error message, and returns 0.  Custom exceptions can be defined using the `exception` keyword, providing specific error types for better error handling.


### References

While SML emphasizes immutability, it does provide references for situations where mutable state is necessary. References are created using the `ref` keyword.

```sml
val x = ref 10; (* x is a reference holding the value 10 *)
!x; (* Access the value of x (dereferencing) - returns 10 *)
x := 20; (* Update the value of x (assignment) *)
!x; (* Access the updated value of x (dereferencing) - returns 20 *)
```

The `!` operator dereferences a reference to obtain its value, and the `:=` operator assigns a new value to a reference.  While references offer mutability, their use should be carefully considered to avoid side effects and maintain the benefits of a functional programming style.


### Higher-order functions

Higher-order functions are functions that take other functions as arguments or return functions as results. This is a key feature of functional programming, enabling powerful abstractions and code reuse.

```sml
fun apply (f, x) = f x; (* A higher-order function *)

fun square (x: real) = x * x;
fun cube (x: real) = x * x * x;

val result1 = apply(square, 5.0); (* result1 = 25.0 *)
val result2 = apply(cube, 2.0);  (* result2 = 8.0 *)
```

`apply` is a higher-order function; it takes a function `f` and a value `x` as arguments and applies `f` to `x`.  This allows you to treat functions as data, enabling more flexible and expressive code.  `List.map`, `List.filter`, and `List.foldl` are examples of built-in higher-order functions.


### Abstract data types

Abstract data types (ADTs) provide a way to define data types along with the operations that can be performed on them, hiding the internal representation.  This promotes data abstraction and information hiding.  ADTs are usually implemented using structures and signatures.

```sml
signature STACK = sig
  type 'a stack;
  val empty : 'a stack;
  val push : 'a * 'a stack -> 'a stack;
  val pop : 'a stack -> 'a * 'a stack;
  val isEmpty : 'a stack -> bool;
end;

structure StackImpl : STACK = struct
  type 'a stack = 'a list;
  val empty = [];
  fun push (x, s) = x :: s;
  fun pop [] = raise Empty
    | pop (x :: xs) = (x, xs);
  fun isEmpty s = s = [];
  exception Empty;
end;
```

This code defines a `STACK` signature specifying the interface for a stack and a `StackImpl` structure implementing that interface using a list. The internal representation (`'a list`) is hidden from the users of the `StackImpl` structure. This abstraction helps maintain the integrity and consistency of the data.  The user only interacts with the functions defined in the signature, preventing direct manipulation of the underlying list representation.


## Example Projects

This section outlines example projects to solidify your understanding of Standard ML and demonstrate its application in different domains.  These are simplified examples, and you can expand upon them to create more complex and feature-rich applications.

### Simple calculator

A simple calculator can perform basic arithmetic operations (+, -, *, /).  This project will involve creating functions for each operation and a main function to handle user input and output.

```sml
fun add (x: real, y: real) = x + y;
fun subtract (x: real, y: real) = x - y;
fun multiply (x: real, y: real) = x * y;
fun divide (x: real, y: real) = if y = 0.0 then raise DivByZero else x / y;

exception DivByZero;

fun calculate (op: string, x: real, y: real) =
  case op of
    "+" => add(x, y)
  | "-" => subtract(x, y)
  | "*" => multiply(x, y)
  | "/" => divide(x, y)
  | _ => raise UnknownOp;

exception UnknownOp;


(* Simple interaction loop (can be improved with more robust input handling) *)
fun main() =
  let
    val input = TextIO.inputLine TextIO.stdIn;
    val [op, xStr, yStr] = String.tokens (fn c => c = ' ') input;
    val x = Real.fromString xStr;
    val y = Real.fromString yStr;
  in
    (case calculate(op, x, y) of
      SOME result => print ("Result: " ^ Real.toString result ^ "\n")
    | NONE => print ("Invalid operation\n"));
    main()
  end
  handle DivByZero => print ("Division by zero error!\n")
       | UnknownOp => print ("Unknown operation!\n");

main();
```

This example uses exception handling for division by zero and unknown operations.  Error handling and input validation should be made more robust in a production-ready calculator.


### Text processing

A text processing program might involve tasks like counting word occurrences, finding specific words, or performing text transformations (e.g., converting to uppercase).  This would involve using string manipulation functions and potentially list processing for handling words.

```sml
fun countWord (word: string, text: string): int =
  let
    val words = String.tokens (fn c => c = ' ') text;
    fun count [] = 0
      | count (h :: t) = if h = word then 1 + count t else count t;
  in
    count words
  end;

val text = "This is a test. This is a sample text.";
val count = countWord("is", text); (* count will be 2 *)
print ("The word 'is' appears " ^ Int.toString count ^ " times.\n");
```

This example demonstrates counting word occurrences. You could extend this to incorporate more advanced text processing tasks.


### Basic game

A simple game like "Guess the Number" can be implemented to illustrate control flow and user interaction.

```sml
fun guessTheNumber () =
  let
    val secretNumber = Random.randInt (1, 100);
    fun play (guessesLeft: int) =
      if guessesLeft = 0 then
        print ("You ran out of guesses! The number was " ^ Int.toString secretNumber ^ ".\n")
      else
        let
          val guessStr = TextIO.inputLine TextIO.stdIn;
          val guess = (case Int.fromString guessStr of
                        SOME i => i
                      | NONE => (print "Invalid input. Please enter a number.\n"; play guessesLeft));
        in
          if guess = secretNumber then
            print ("Congratulations! You guessed the number in " ^ Int.toString (100 - guessesLeft) ^ " tries.\n")
          else if guess < secretNumber then
            print ("Too low! Try again.\n"; play (guessesLeft - 1))
          else
            print ("Too high! Try again.\n"; play (guessesLeft - 1))
        end;
  in
    print ("Welcome to Guess the Number!\n");
    print ("I've chosen a number between 1 and 100.\n");
    play 10
  end;


guessTheNumber();
```

This “Guess the Number” game provides a basic example of implementing game logic using functions and recursion for game loop.  You could enhance this with features such as difficulty levels or score tracking.  Remember to include `use "Random";` at the beginning for random number generation.  These examples provide a starting point. You can expand upon them to create more sophisticated and engaging projects.

