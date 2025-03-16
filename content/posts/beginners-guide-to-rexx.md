+++
title = "Beginner's Guide to Rexx"
date = 2024-12-30
toc = true
readTime = true
+++

## Introduction to Rexx

### What is Rexx?

REXX (REstructured eXtended eXecutor) is a powerful, interpreted, general-purpose programming language known for its readability, ease of use, and portability.  Unlike compiled languages like C or C++, Rexx code is not translated into machine code before execution. Instead, a Rexx interpreter reads and executes the code directly.  This makes it ideal for rapid prototyping and scripting tasks. Rexx is particularly well-suited for string manipulation, text processing, and system administration tasks.  Its syntax is designed to be easily understood, even by those with limited programming experience, making it a great choice for beginners.  Many operating systems, including IBM z/OS, OS/2, and various Unix-like systems, have native Rexx interpreters.

### Why learn Rexx?

Learning Rexx offers several advantages:

* **Easy to learn:**  Rexx's clear syntax and straightforward commands make it relatively easy to pick up, even for novice programmers.  The learning curve is significantly gentler than many other programming languages.
* **Highly portable:**  Rexx interpreters are available on a wide range of operating systems, meaning your code can often be run without modification across different platforms.
* **Excellent for scripting:**  Its ability to easily interact with the operating system makes it a perfect choice for automating repetitive tasks, managing files, and performing system administration duties.
* **Powerful string manipulation:** Rexx provides a rich set of built-in functions specifically designed for manipulating strings, making it exceptionally effective for text processing and data transformation.
* **Good for rapid prototyping:** The interpreted nature of Rexx allows for quick development and testing cycles, facilitating rapid prototyping and iterative development.

### Rexx's strengths and weaknesses

**Strengths:**

* **Readability and ease of use:**  The syntax is clean and intuitive.
* **Portability:**  Runs on many different operating systems.
* **Excellent string handling:**  Powerful built-in functions for string manipulation.
* **Good for scripting and automation:**  Easy interaction with the operating system.
* **Interactive interpreter:** Allows for immediate feedback and experimentation.


**Weaknesses:**

* **Performance:** Being an interpreted language, Rexx can be slower than compiled languages for computationally intensive tasks.
* **Limited libraries:** Compared to languages like Python or Java, Rexx has a smaller standard library.  However, many extensions and custom libraries exist depending on the specific Rexx implementation.
* **Smaller community:**  The community surrounding Rexx is smaller than those of other popular programming languages, meaning that finding support and resources might sometimes be more challenging.


### Setting up your Rexx environment

The steps for setting up your Rexx environment depend on your operating system.  However, the general process usually involves these steps:

1. **Download a Rexx interpreter:**  Find a Rexx interpreter appropriate for your operating system.  Many implementations are available online, both free and commercial. Popular choices include Regina Rexx (a free, open-source interpreter available for multiple platforms), and the native Rexx interpreters provided with some IBM operating systems.

2. **Install the interpreter:** Follow the installation instructions provided with the downloaded interpreter. This usually involves unpacking the archive and possibly adding the interpreter's directory to your system's PATH environment variable.  This ensures that you can run Rexx programs from any directory.

3. **Verify the installation:** Open a command prompt or terminal and type `rexx` or the appropriate command to run your Rexx interpreter.  If the installation was successful, you should see the Rexx prompt (often `>` or `>>`).

4. **Create a simple Rexx program:** Create a text file (e.g., `hello.rex`) with a simple Rexx program (such as `say 'Hello, world!'`).

5. **Run your program:** Execute your program from the command line using the command  `rexx hello.rex` (or the appropriate command for your Rexx interpreter).  If everything is working correctly, you should see "Hello, world!" printed on the console.

Remember to consult the documentation for your specific Rexx interpreter for detailed installation instructions and any platform-specific considerations.


## Basic Rexx Syntax

### Comments and whitespace

Rexx uses the `/*` and `*/` characters to denote block comments.  Anything between these characters will be ignored by the interpreter.  Single-line comments are also possible using the `/*` at the beginning of the line, or a `/* ... */`  on a single line.   Whitespace (spaces, tabs, newlines) is generally ignored by Rexx, except within string literals. This allows for well-formatted, readable code.  Indentation is not required by the interpreter but is strongly recommended to improve code readability, especially for complex programs.


### Variables and data types

Rexx variables are implicitly declared; you don't need to specify a type.  A variable is created the first time it is used.  Rexx is dynamically typed; the type of a variable can change during program execution.  Rexx primarily works with strings.  Numbers are treated as strings unless explicitly used in arithmetic operations.


### Assignment statements

The assignment operator is `=`.  To assign a value to a variable, use the following syntax:

```rexx
variable_name = value
```

For example:

```rexx
name = "Alice"
age = 30
```

Note that even the numerical value 30 is stored as a string.


### Operators (Arithmetic, logical, comparison)

Rexx supports a variety of operators:

**Arithmetic Operators:**

* `+` (addition)
* `-` (subtraction)
* `*` (multiplication)
* `/` (division)
* `%` (modulo)
* `**` (exponentiation)


**Logical Operators:**

* `&` (AND)
* `|` (OR)
* `¬` (NOT)


**Comparison Operators:**

* `=` (equals)
* `\=` (not equals)
* `<` (less than)
* `>` (greater than)
* `<=` (less than or equals)
* `>=` (greater than or equals)


**Example:**

```rexx
x = 10
y = 5
sum = x + y           /* sum will be 15 (as a string) */
difference = x - y     /* difference will be 5 (as a string) */
is_equal = (x = y)    /* is_equal will be 0 (false) */
is_greater = (x > y)   /* is_greater will be 1 (true) */
```


### Basic Input/Output (say, pull)

* **`say`:** The `say` instruction sends output to the console.

```rexx
say "Hello, world!"
say name  /* Outputs the value of the name variable */
```

* **`pull`:** The `pull` instruction reads a line of input from the console.  It stores the input as a string in the variable specified after `pull`.

```rexx
pull username
say "Welcome, " username "!"
```

In this example, the program will prompt the user to enter a username and then greet the user using the entered username.  Multiple values can be read into multiple variables in a single `pull` statement, separated by spaces.  For example `pull var1 var2 var3` will read three values into `var1`, `var2`, and `var3` separated by spaces.  If fewer values than variables are entered, the remaining variables will be assigned empty strings.


## Control Structures

### Conditional statements (if-then-else)

Rexx uses `if-then-else` statements to control the flow of execution based on conditions.  The syntax is as follows:

```rexx
if condition then
  /* statements to execute if the condition is true */
else
  /* statements to execute if the condition is false */
endif
```

You can omit the `else` block if you only need to execute statements when the condition is true.  Multiple conditions can be chained using `elseif`:


```rexx
x = 10
if x > 20 then
  say "x is greater than 20"
elseif x > 10 then
  say "x is greater than 10"
else
  say "x is 10 or less"
endif
```

Conditions are evaluated as being true if they result in a non-zero value, and false if they result in a zero value.


### Looping structures (do-while, do-until, do-forever)

Rexx provides several ways to create loops:

* **`do while`:** This loop continues as long as the specified condition is true.

```rexx
i = 1
do while i <= 5
  say i
  i = i + 1
enddo
```

* **`do until`:** This loop continues until the specified condition becomes true.

```rexx
i = 1
do until i > 5
  say i
  i = i + 1
enddo
```

* **`do forever`:** This loop runs indefinitely until explicitly terminated using a `leave` statement (see below).

```rexx
i = 1
do forever
  say i
  i = i + 1
  if i > 5 then leave
enddo
```


### Nested loops and conditionals

Loops and conditional statements can be nested within each other to create more complex control structures.  The following example shows a nested loop:

```rexx
do i = 1 to 3
  do j = 1 to 2
    say "i = " i ", j = " j
  enddo
enddo
```

Similarly, you can nest conditional statements within loops or other conditionals.


### Exiting loops prematurely

The `leave` statement can be used to exit a loop prematurely.  This is particularly useful in `do forever` loops or when a certain condition is met within a loop that requires immediate termination.  The `leave` statement only exits the innermost loop it's contained in.

```rexx
i = 1
do forever
  say i
  i = i + 1
  if i > 5 then leave
enddo
```

In this example, the loop will terminate when `i` becomes greater than 5.  You can also use `iterative` and `conditional` statements within a loop to control the iteration process.  `iterative` allows skipping an iteration and proceeding to the next one and `conditional` checks for a condition and will leave the loop if true.  Check your specific Rexx implementation documentation to see if it supports these features.


## Working with Strings

Rexx excels at string manipulation.  Its built-in functions make it easy to work with text data.

### String concatenation

Strings in Rexx are concatenated simply by placing them next to each other.  The `||` operator can also be explicitly used for concatenation.

```rexx
first_name = "John"
last_name = "Doe"
full_name = first_name last_name   /* full_name will be "JohnDoe" */
full_name2 = first_name || " " || last_name  /* full_name2 will be "John Doe" */
```

Note that no explicit operator is needed; juxtaposition automatically concatenates.


### String manipulation functions (substr, left, right, etc.)

Rexx offers numerous built-in functions for manipulating strings.  Here are some commonly used ones:

* **`substr(string, start, length)`:** Extracts a substring of a specified length from a string, starting at a given position.  `start` is 1-based (the first character is at position 1).

```rexx
my_string = "Hello, world!"
substring = substr(my_string, 7, 5)  /* substring will be "world" */
```

* **`left(string, length)`:** Extracts the leftmost `length` characters from a string.

```rexx
my_string = "Hello, world!"
left_part = left(my_string, 5)     /* left_part will be "Hello" */
```

* **`right(string, length)`:** Extracts the rightmost `length` characters from a string.

```rexx
my_string = "Hello, world!"
right_part = right(my_string, 5)    /* right_part will be "world!" */
```

* **`length(string)`:** Returns the length of a string.

```rexx
my_string = "Hello, world!"
string_length = length(my_string)  /* string_length will be 13 */
```

* **`strip(string)`:** Removes leading and trailing blanks from a string.   Check your implementation for variations (e.g., `leading()`, `trailing()` for removing only leading or trailing spaces).


* **`translate(string, to_string, from_string)`:** Replaces characters in a string.


* **`upper(string)`:** Converts a string to uppercase.

* **`lower(string)`:** Converts a string to lowercase.


Many other string functions are available; consult your Rexx interpreter's documentation for a complete list.


### Searching and replacing within strings

Rexx doesn't have a single built-in function to directly search and replace, but it can be accomplished using a combination of functions like `pos` (finds the position of a substring) and `substr` (extracts a substring), combined with concatenation.  Alternatively, regular expressions (if supported by your Rexx implementation) provide more powerful search and replace capabilities.


### String comparisons

String comparisons in Rexx are case-sensitive by default.  The comparison operators (`=`, `\=`, `<`, `>`, `<=`, `>=`) can be used to compare strings.  The comparison is based on the lexicographical order of characters.  For case-insensitive comparisons, you might need to convert strings to uppercase or lowercase using `upper()` and `lower()` functions before comparison.


```rexx
string1 = "apple"
string2 = "Apple"
if string1 = string2 then
  say "Strings are equal (case-sensitive)"
else
  say "Strings are not equal (case-sensitive)"
endif

if upper(string1) = upper(string2) then
  say "Strings are equal (case-insensitive)"
endif
```



## Working with Numbers

While Rexx primarily treats data as strings, it provides robust support for numerical operations.

### Arithmetic operations

Rexx supports standard arithmetic operations:

* **Addition:** `+`
* **Subtraction:** `-`
* **Multiplication:** `*`
* **Division:** `/`
* **Modulo (remainder):** `%`
* **Exponentiation:** `**`

These operators work as expected, but it's crucial to remember that the *results* of arithmetic operations are still strings.  If you need to perform further arithmetic operations, ensure that the results are treated numerically (e.g., using functions that expect numeric input).


```rexx
x = 10
y = 5
sum = x + y       /* sum is "15" (a string) */
product = x * y    /* product is "50" (a string) */
division = x / y   /* division is "2" (a string) */
```

To perform further calculations, you might need explicit type conversion to numeric.  The specific method depends on the Rexx implementation; some might automatically convert strings to numbers within arithmetic expressions; others might require an explicit function (refer to your specific Rexx documentation).


### Number formatting

Rexx offers functions for formatting numbers as strings.  The specific functions available vary depending on the Rexx implementation.  Common formatting options include specifying the number of decimal places, adding commas as thousands separators, and controlling the overall appearance.  Check the documentation for your particular Rexx interpreter to see what formatting functions are available.  For example, some implementations might offer a `format()` function with various options.  This ensures numerical outputs are presented in a user-friendly and consistent manner.  Consider exploring any available libraries that offer enhanced formatting capabilities.


### Mathematical functions

Rexx provides a set of built-in mathematical functions.  The availability and names of these functions might vary slightly across Rexx implementations. Common functions often include:

* **`abs(number)`:** Returns the absolute value of a number.
* **`sin(number)`:** Returns the sine of a number (in radians).
* **`cos(number)`:** Returns the cosine of a number (in radians).
* **`tan(number)`:** Returns the tangent of a number (in radians).
* **`atan(number)`:** Returns the arctangent of a number (in radians).
* **`sqrt(number)`:** Returns the square root of a number.
* **`log(number)`:** Returns the natural logarithm of a number.
* **`log10(number)`:** Returns the base-10 logarithm of a number.
* **`exp(number)`:** Returns the exponential of a number (e raised to the power of the number).
* **`max(number1, number2, ...)`:** Returns the maximum of a set of numbers.
* **`min(number1, number2, ...)`:** Returns the minimum of a set of numbers.
* **`round(number)`:** Rounds a number to the nearest integer.
* **`trunc(number)`:** Truncates a number to an integer (removes the fractional part).

Remember that these functions typically expect numeric input and return numeric values (although the returned value will ultimately be represented as a string).  Ensure proper handling for cases with non-numeric input to avoid unexpected errors.





## Arrays and Lists

Rexx doesn't have built-in array structures in the same way as some other languages (like Python or Java). However, you can effectively simulate arrays and lists using a technique involving string manipulation and indexing.  Rexx's strength in string manipulation makes this approach quite efficient.

### Declaring and initializing arrays

Rexx "arrays" are typically implemented using a single string variable where elements are separated by a delimiter (often a space or comma).  There's no explicit declaration; the array is created when you first assign values to it.

```rexx
my_array = "10 20 30 40 50"  /* Array of numbers, space-delimited */
my_list = "apple,banana,orange"  /* List of strings, comma-delimited */
```


### Accessing array elements

Individual elements are accessed using `substr` along with the knowledge of the delimiter and element positions.  You'll need to carefully track the lengths of each element to compute the correct starting positions.

```rexx
/* Accessing elements from my_array */
element1 = substr(my_array, 1, 2)    /* element1 will be "10" */
element2 = substr(my_array, 4, 2)    /* element2 will be "20" */
/* ...and so on...  Requires careful calculation of starting positions based on the delimiter and element lengths */

/* Example using word() to simplify access (space-delimited array) */
element3 = word(my_array, 3)  /* element3 will be "30" */


```

The `word()` function (illustrated above), which extracts the nth word from a string (where words are separated by spaces), is a frequently used utility when working with arrays or lists represented as strings. The `words()` function returns the number of words in a string.


### Iterating through arrays

Looping through a Rexx "array" involves iterating through the string and extracting each element.  You'll often need functions like `word()` (for space-delimited arrays) and a loop to handle the array elements sequentially.

```rexx
num_words = words(my_array)  /* Get the number of words (elements) */
do i = 1 to num_words
  element = word(my_array, i)
  say "Element " i ": " element
enddo
```

For comma-delimited lists, you'd need to use `pos` to find the comma positions and `substr` to extract each element. This is slightly more complex than using space delimiters.


### Using arrays effectively

While not true arrays, the string-based approach offers simplicity for smaller datasets.  For large or complex data structures, consider alternative methods:

* **External files:** Store the data in a text file and process it line by line.
* **Custom data structures:** Implement more sophisticated data structures if needed, but this typically involves writing more complex code.

Remember that error handling (e.g., checks for valid array indices) is essential to prevent unexpected behavior or crashes.  The string-based implementation of arrays in Rexx is effective for small-scale tasks, but it’s important to choose a more efficient approach for larger data sets or if the array manipulation becomes overly complex.



## Functions and Subroutines

Rexx supports functions and subroutines, promoting modularity and code reusability.  While Rexx doesn't have a strict distinction between functions and subroutines in the same way as some other languages (e.g., a function *must* return a value, a subroutine doesn't), the terms are used here to describe procedures that either return a value or don't.

### Defining and calling functions

Functions are defined using the `function` keyword, followed by the function name and a block of code enclosed within `do` and `end`.

```rexx
/* Function to add two numbers */
add_numbers: procedure
  arg num1, num2
  return num1 + num2
end add_numbers

/* Calling the function */
result = add_numbers(5, 3)  /* result will be "8" */
say result
```

Subroutines (procedures that don't explicitly return a value) follow a similar structure but omit the `return` statement.


### Passing parameters to functions

Parameters are passed to functions by specifying them within parentheses after the function name during the call.  Inside the function, parameters are accessed using the `arg` keyword.

```rexx
greet: procedure
  arg name
  say "Hello, " name "!"
end greet

greet("Alice")  /* Calls the greet subroutine with the parameter "Alice" */
```


### Returning values from functions

Functions return values using the `return` statement. The returned value can then be assigned to a variable or used in other expressions.  If a function doesn't have a `return` statement, or the return value is omitted, the function effectively acts like a subroutine; the returned value will be an empty string in that case (check your specific Rexx implementation for exact behavior).

```rexx
square: procedure
  arg num
  return num * num
end square

result = square(4) /* result will be "16" */
say result
```


### Benefits of modular programming

Using functions and subroutines provides several advantages:

* **Code Reusability:** Functions can be called from multiple parts of the program, avoiding redundant code.
* **Improved Readability:** Breaking down complex tasks into smaller, more manageable functions makes the code easier to understand and maintain.
* **Easier Debugging:** Isolating code into functions simplifies the debugging process, as you can test individual functions independently.
* **Better Organization:**  Modular code is better organized, promoting clearer structure and improved maintainability.  This is especially important for larger and more complex Rexx programs.
* **Enhanced Collaboration:** Modular design facilitates teamwork as different developers can work on separate functions or modules concurrently.


Remember that good naming conventions for functions and subroutines are essential for readability and maintainability.  Choose descriptive names that clearly indicate the purpose of the function or subroutine.  Using comments to explain the function's purpose and parameters is also a good practice.


## Advanced Rexx Techniques

This section explores more advanced Rexx capabilities, enhancing your ability to create robust and powerful programs.

### Working with external files

Rexx provides several ways to interact with external files:

* **Opening and closing files:**  Functions like `open` and `close` are used to handle file I/O.  The specific functions and their parameters might vary slightly between different Rexx implementations, so consult your interpreter's documentation.  Typically, you'll specify the filename and access mode (e.g., read, write, append).

* **Reading from files:** The `linein` function reads a line from a file. You'll generally use it within a loop to read the entire file content line by line.

* **Writing to files:** The `lineout` function writes a line to a file.  You'll typically use it within a loop to write data to the file.

* **Error handling:**  Always check for file opening errors (e.g., file not found) and handle them gracefully to prevent program crashes.


Example (Illustrative - specific functions may differ slightly):

```rexx
filename = "mydata.txt"
file_handle = open(filename)
if file_handle = 0 then
  say "Error opening file!"
  exit
endif

do while lines(file_handle) > 0
  line = linein(file_handle)
  say line
enddo

close(file_handle)
```


### Exception handling

While Rexx doesn't have a structured exception handling mechanism like `try-catch` blocks in some languages, you can implement error handling using conditional statements and checks for error codes returned by functions.  For instance, check the return values of file I/O operations or other potentially error-prone functions and handle errors appropriately (e.g., display error messages, log errors, or take corrective actions).


### Debugging Rexx programs

Debugging Rexx programs typically involves:

* **Print statements:** Insert `say` statements strategically in your code to display the values of variables or monitor the flow of execution.

* **Interactive debugging:** Some Rexx interpreters provide interactive debuggers allowing you to step through the code line by line, examine variables, and set breakpoints.  Check your interpreter's documentation to see if a debugger is included.

* **Error messages:**  Carefully examine any error messages generated by the interpreter. They often provide valuable clues about the source of the problem.  

* **Code review:** Have another developer review your code for potential errors or areas for improvement.


### Interfacing with other systems

Rexx can interact with other systems through various methods:

* **Command execution:** The `address` instruction can execute operating system commands and retrieve their output.  This is particularly useful for automating tasks that involve interacting with the operating system (e.g., running other programs, executing shell commands).

* **External programs:**  Rexx programs can call external programs or functions (DLLs, shared libraries, etc.).  The specific mechanisms vary depending on the operating system and the Rexx interpreter.

* **APIs and libraries:**  Depending on the Rexx implementation and available libraries, you might be able to interface with system APIs or specific libraries to extend the functionalities of your Rexx programs.  Consult your Rexx implementation's documentation for specific details.


Remember that security considerations are crucial when interfacing with external systems.  Always validate user inputs to prevent security vulnerabilities and follow secure coding practices to avoid any security risks when interacting with other programs or systems.


## Example Programs

This section provides example programs demonstrating various Rexx features.  Remember that the specific functions and their behavior might have minor variations depending on your Rexx interpreter.

### Simple calculator

This program performs basic arithmetic operations:

```rexx
say "Enter the first number:"
pull num1
say "Enter the second number:"
pull num2
say "Enter the operation (+, -, *, /):"
pull op

if op = "+" then
  result = num1 + num2
elseif op = "-" then
  result = num1 - num2
elseif op = "*" then
  result = num1 * num2
elseif op = "/" then
  if num2 = 0 then
    say "Error: Cannot divide by zero!"
  else
    result = num1 / num2
  endif
else
  say "Error: Invalid operator!"
endif

if result <> "" then /* Check if there was a valid result */
  say "Result: " result
endif
```


### String manipulation program

This program demonstrates several string manipulation functions:

```rexx
say "Enter a string:"
pull str

len = length(str)
say "Length of the string: " len

upper_str = upper(str)
say "Uppercase string: " upper_str

lower_str = lower(str)
say "Lowercase string: " lower_str

substr1 = substr(str, 1, 5)  /* Extract first 5 characters */
say "Substring (first 5 characters): " substr1

pos_a = pos("a", str)  /* Find the position of "a" */
if pos_a > 0 then
  say "Position of 'a': " pos_a
else
  say "'a' not found in the string."
endif
```



### File processing program

This program reads data from a file, processes it, and writes the results to another file:

```rexx
input_file = "input.txt"
output_file = "output.txt"

infile = open(input_file)
if infile = 0 then
  say "Error opening input file!"
  exit
endif

outfile = open(output_file, "w") /* Open for writing */
if outfile = 0 then
  say "Error opening output file!"
  close(infile)
  exit
endif


do while lines(infile) > 0
  line = linein(infile)
  /* Process the line (e.g., convert to uppercase) */
  processed_line = upper(line)
  lineout outfile, processed_line
enddo

close(infile)
close(outfile)
say "File processing complete. Results written to " output_file
```

Remember to create an `input.txt` file in the same directory before running this program.  The program will create an `output.txt` file with the uppercase version of the input.  Error handling is included; always consider robust error handling in your own programs.


These examples provide a starting point. You can modify and expand upon them to explore more advanced concepts and build more complex Rexx applications. Remember to consult the documentation for your specific Rexx interpreter for details on functions and syntax.


## Resources and Further Learning

This section provides links and suggestions for continued learning and exploration of the Rexx programming language.  Note that the availability and specific URLs of online resources can change over time.

### Online Rexx documentation

Finding comprehensive, up-to-date online documentation for Rexx can be challenging due to the language's history and the existence of various implementations.  However, several resources often prove useful:

* **Regina Rexx:** If you're using the Regina Rexx interpreter, their website usually contains documentation and tutorials. Search online for "Regina Rexx documentation" to find the most current link.

* **IBM documentation (for z/OS and other IBM systems):** If you're working with Rexx on IBM platforms (like z/OS),  search the IBM Knowledge Center or their official documentation websites for Rexx documentation specific to your operating system and Rexx version.

* **Open-source implementations:** Some open-source Rexx projects might have their own documentation pages on platforms like GitHub.


It's often helpful to search online for specific Rexx functions or features you need help with, as many tutorials and example code snippets are available on various websites and programming forums.


### Rexx communities and forums

While not as large as communities for some more modern languages, there are still online communities where you can find assistance and connect with other Rexx programmers:

* **Stack Overflow:**  Search for Rexx-related questions on Stack Overflow; you'll find a number of helpful answers and discussions.

* **Rexx mailing lists or forums:** Search online for "Rexx mailing list" or "Rexx forum" to find older forums or mailing lists that might still be active.  These older forums might contain valuable archived information and discussions on Rexx programming.


Engaging with these communities can provide valuable support, allow you to ask questions, and share your knowledge with other Rexx developers.


### Books and tutorials on Rexx

Finding dedicated books solely on Rexx might be more challenging than for other programming languages.  However, you can often find relevant information in books covering broader topics like scripting or system administration that include Rexx as one of the scripting languages discussed.  Search on Amazon or other online book retailers using keywords like "Rexx programming," "Rexx tutorial," or "Rexx scripting" to see what's available.


Additionally, searching online for "Rexx tutorial PDF" or "Rexx tutorial beginners" can yield various free tutorials and learning materials.  Remember to check the publication date or last update of any tutorial you find to ensure it's up-to-date and relevant.  Many older tutorials might still be useful, but they might not cover the newest features or best practices.

