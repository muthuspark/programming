+++
title = "Beginner's Guide to PureScript"
date = 2025-01-11
toc = true
readTime = true
+++

## Introduction to PureScript

### What is PureScript?

PureScript is a strongly-typed, functional programming language that compiles to JavaScript.  It's designed to be a pragmatic alternative to JavaScript, offering the benefits of a statically-typed language with a focus on purity and immutability while targeting the ubiquitous JavaScript ecosystem. This means you can write PureScript code and compile it to JavaScript that can run in web browsers or Node.js environments.  PureScript leverages many concepts from Haskell, known for its powerful type system and elegant functional paradigm, but with a syntax designed for better readability and a more approachable learning curve.  The compiler performs extensive type checking at compile time, catching many potential errors before runtime, leading to more robust and maintainable applications.


### Why use PureScript?

PureScript offers several compelling advantages for developers:

* **Increased Reliability:**  The strong type system catches errors early in the development process, leading to fewer runtime bugs and more stable applications.
* **Improved Code Readability:** PureScript's functional paradigm and concise syntax encourage the creation of modular, easily understandable code.  Immutability reduces the risk of unexpected side effects, making code easier to reason about.
* **Enhanced Maintainability:**  The well-defined types and functional nature of PureScript lead to more maintainable codebases, reducing the cost of long-term development and maintenance.
* **JavaScript Interoperability:** PureScript compiles to JavaScript, allowing seamless integration with existing JavaScript libraries and frameworks.  You can leverage the vast JavaScript ecosystem while enjoying the benefits of PureScript.
* **Performance:** The compiled JavaScript is often quite efficient, and the compiler offers various optimization options.


### PureScript vs. other languages

PureScript shares similarities with other languages but distinguishes itself in key aspects:

* **Compared to JavaScript:** PureScript addresses many of JavaScript's weaknesses related to type safety and maintainability. It offers a more structured approach and better tooling, making it suitable for larger and more complex projects.  However, it requires learning a new language and tooling, although the payoff in terms of code quality and maintainability is often substantial.

* **Compared to Haskell:** PureScript is inspired by Haskell but is designed to be more practical for web development. Its syntax is generally considered more approachable than Haskell's, and its JavaScript interoperability is a key advantage.

* **Compared to TypeScript:** While both TypeScript and PureScript add static typing to JavaScript development, PureScript takes a fundamentally different approach. TypeScript is a superset of JavaScript, gradually adding types. PureScript is a completely separate language that compiles to JavaScript, leading to more comprehensive type checking and a more functional style.


### Setting up your environment

To start developing with PureScript, you'll need to install several tools:

1. **Node.js and npm:** PureScript relies on Node.js and npm (Node Package Manager) for building and managing dependencies. Download and install the latest LTS version of Node.js from [https://nodejs.org/](https://nodejs.org/).  This automatically installs npm.

2. **spago:** Spago is the official PureScript package manager. Install it globally using npm:  `npm install -g spago`

3. **A Code Editor:** Choose a code editor with good PureScript support. Popular options include VS Code with the PureScript extension, Atom with the `ide-purescript` package, or Sublime Text with appropriate plugins.

4. (Optional) **A Build System:** While not strictly required for small projects, a build system like `npm` or `spago` can significantly improve the workflow for larger projects.

After installing these tools, you're ready to create your first PureScript project using `spago init`. Refer to the Spago documentation for details on creating and managing PureScript projects.  Further details on setting up specific editors and build systems are available in separate sections of this manual.


## Basic Syntax and Concepts

### Data Types

PureScript boasts a rich type system that is statically checked at compile time.  Understanding these types is crucial for writing correct and efficient PureScript code. Some fundamental data types include:

* **Numbers:**  Represent numerical values (e.g., `1`, `3.14`). PureScript distinguishes between integers (`Int`) and floating-point numbers (`Number`).

* **Booleans:** Represent truth values (`true` and `false`).

* **Strings:** Represent text enclosed in double quotes (e.g., `"Hello, world!"`).

* **Arrays:** Ordered collections of values of the same type.  Declared using square brackets (e.g., `[1, 2, 3]`).

* **Records:**  Similar to objects in JavaScript, but with named fields and a statically-defined structure.  Defined using curly braces (e.g., `{ name : "Alice", age : 30 }`).

* **Maybe:** Represents a value that might be present or absent.  Has two constructors: `Nothing` (representing absence) and `Just` (representing a present value, e.g., `Just 5`).  Crucial for handling potential errors gracefully.

* **Either:** Represents a value that can be one of two possible types.  Useful for representing success or failure scenarios.  Has two constructors: `Left` (representing a failure) and `Right` (representing success).

* **Custom Data Types:** PureScript allows you to define your own algebraic data types using `data` declarations. This is a powerful feature for modeling complex data structures and behaviors.


### Functions

Functions are first-class citizens in PureScript.  They are defined using the `=` operator:

```purescript
add :: Number -> Number -> Number
add x y = x + y
```

This defines a function `add` that takes two numbers (`Number`) as arguments and returns a number.  The type signature `:: Number -> Number -> Number` is crucial for static type checking and is often inferred by the compiler.  Function application is done by simply placing the arguments next to the function name: `add 2 3`.


### Immutability

PureScript values are immutable.  Once a value is created, its value cannot be changed.  Instead of modifying values, new values are created. This significantly simplifies reasoning about code and reduces the risk of unexpected side effects.

For instance, if you have a list `[1, 2, 3]`, you can't modify it in place. To add an element, you create a new list: `[1, 2, 3, 4]`. This immutability is a core principle of functional programming and a key benefit of using PureScript.


### Type Inference

PureScript's compiler is very good at inferring types.  While you can explicitly specify types (as shown in the `add` function example above), in many cases, the compiler can deduce the types automatically:

```purescript
greet name = "Hello, " <> name
```

The compiler will infer that `greet` takes a `String` argument and returns a `String`. This reduces boilerplate and improves code readability.


### Pattern Matching

Pattern matching is a powerful feature in PureScript for extracting information from data structures.  It allows you to define different behaviors based on the structure of the input data:

```purescript
processMaybe :: Maybe Number -> String
processMaybe Nothing  = "Nothing here!"
processMaybe (Just n) = "The number is: " <> show n
```

This function uses pattern matching to handle the `Maybe` type.  If the input is `Nothing`, it returns "Nothing here!".  If it's `Just n`, it extracts the number `n` and returns a string containing the number.  Pattern matching significantly improves code clarity and reduces the need for explicit conditional statements (like `if-else`).


## Working with the PureScript Ecosystem

### Package Management with Spago

Spago is the official package manager for PureScript. It simplifies the process of managing dependencies, building projects, and interacting with the PureScript ecosystem.  Here's a brief overview of its key functionalities:

* **Creating Projects:**  Use `spago init` to create a new PureScript project. This command sets up the basic project structure, including a `spago.dhall` file (which describes your project's dependencies) and a directory for your source code.

* **Adding Dependencies:**  Spago uses the `spago install` command to add dependencies to your project.  Dependencies are specified in the `spago.dhall` file using a declarative configuration format called Dhall.  This allows precise control over version numbers and dependency resolution.

* **Building Projects:** Spago handles the compilation of your PureScript code into JavaScript. Use `spago build` to compile your project. This command automatically fetches, builds, and compiles your dependencies.

* **Managing Versions:** Spago's use of Dhall allows for precise specification of package versions, including constraints and ranges.  This helps maintain consistency and avoid conflicts between different versions of libraries.

* **Running Tests:** PureScript projects often use a testing framework like `purescript-test`.  Spago integrates with testing frameworks and provides commands to run your tests.


### Common Libraries and Modules

The PureScript ecosystem offers a wide range of libraries and modules that extend the core language's capabilities. Some essential ones include:

* **purescript-arrays:** Provides functions for working with arrays.

* **purescript-strings:** Offers functions for manipulating strings.

* **purescript-maybe:**  Provides functions for working with the `Maybe` type safely.

* **purescript-either:** Offers functions for working with the `Either` type for handling errors or alternative results.

* **purescript-effect:** Provides tools for working with effects, enabling interaction with the outside world (e.g., I/O operations).

* **purescript-foreign:** Allows interoperability with JavaScript libraries and APIs.

* **purescript-react:**  A library that allows building user interfaces using React.

* **purescript-node:** Enables building backend applications in PureScript that run on Node.js.


You can find these and many other libraries on the PureScript package repository ([https://pursuit.purescript.org/packages](https://pursuit.purescript.org/packages)).


### Building and Running Your Application

The process of building and running a PureScript application depends on its type (web application, Node.js application, etc.).  Generally, you'll follow these steps:

1. **Project Setup:**  Create a new project using `spago init`. Add any necessary dependencies via `spago install`.

2. **Write Code:** Develop your PureScript code in the appropriate modules within your project structure.

3. **Build the Project:** Use `spago build` to compile your PureScript code into JavaScript.  Spago will manage dependencies and handle the compilation process.

4. **Run the Application:** The method for running the application will vary based on the project type.  For web applications, you may need to serve the generated JavaScript files using a web server (e.g., using `spago serve`). For Node.js applications, you would run the compiled JavaScript using Node.js.

5. **Testing:**  Utilize `spago test` to execute your unit tests written with a framework like `purescript-test`.

It's crucial to consult the documentation for specific libraries and frameworks to understand the unique build and execution steps they might require.  Many projects provide build scripts or instructions in their READMEs to streamline the process.


## Advanced Concepts

### Type Classes

Type classes in PureScript provide a way to define interfaces or contracts that different types can implement.  This allows for writing generic functions that operate on various types, as long as those types adhere to the specified interface.  A type class declaration defines a set of functions (often called *methods*) that must be implemented by any type that is an *instance* of the type class.

For example, the `Show` type class defines a `show` function that converts a value to a string representation.  If a type is an instance of `Show`, you can use the `show` function on values of that type.  This enables polymorphism, allowing the same function to work with different types without explicit conditional logic.  Similarly, the `Eq` type class provides functions for comparing equality, while `Ord` adds ordering capabilities.  Understanding type classes is fundamental to writing reusable and generic PureScript code.


### Functors, Applicatives, and Monads

These are higher-kinded type classes that are fundamental to functional programming in PureScript and provide powerful abstractions for working with computations.

* **Functor:**  A Functor is a type that implements the `map` function, which allows applying a function to the value inside the type.  This lets you transform the value within a context (like `Maybe`, `Array`, etc.) without needing to directly handle the context itself.

* **Applicative:** An Applicative is a Functor that also provides a way to apply a function wrapped in a context to a value wrapped in a context. This allows combining functions and values which are wrapped up in something like a `Maybe` or `Either`.

* **Monad:** A Monad is an Applicative that also provides a way to chain computations together.  This is done via the `bind` function (often written as `>>=`), which allows you to sequence operations, where the result of one operation determines the input to the next.  Monads provide a structured way to handle side effects and manage complex workflows while maintaining code readability.  The `Aff` effect (described below) is a prime example of a monad in PureScript.


### Effect Handling with `Aff`

`Aff` (Asynchronous Effect) is a monad in PureScript that provides a way to perform asynchronous operations while still maintaining the benefits of purity and immutability.  Instead of directly performing side effects (like network requests or DOM manipulation), actions that would otherwise cause side-effects are represented as computations within the `Aff` monad.  This allows you to structure and combine asynchronous operations using monadic functions like `>>=` (bind) and `<$>` (map), making asynchronous code easier to read and reason about.  PureScript's approach to effects through monads allows you to write code that looks synchronous but runs asynchronously.


### Working with the DOM

Interacting with the Document Object Model (DOM) is often necessary for web applications. In PureScript, this is typically done using a library that provides bindings to JavaScript's DOM APIs.  `purescript-dom` is a popular choice.  Using `Aff`, these interactions are handled within the monad, keeping your code pure while managing asynchronous DOM operations.  This often involves selecting elements, manipulating their attributes, attaching event handlers, and updating content.  By using `Aff`, you can manage complex sequences of DOM manipulation while keeping your code clean and free from problematic side-effects in the PureScript code itself. The complexity of handling callbacks and managing asynchronous operations is abstracted away by the `Aff` monad, leading to more readable and reliable code.


## Building a Simple Application

This section guides you through building a small PureScript application to solidify your understanding of the concepts covered earlier.  We'll create a simple application that fetches data from a public API and displays it on the webpage.


### Project Setup

1. **Create a Project:** Use Spago to create a new project:  `spago init my-simple-app`

2. **Install Dependencies:** We'll need libraries for DOM manipulation, networking, and potentially testing. Add them using `spago install`:

   ```bash
   spago install purescript-dom purescript-aff purescript-node purescript-maybe purescript-result
   ```

3. **Project Structure:** Your project will have a directory structure similar to this:

   ```
   my-simple-app/
   ├── src/
   │   └── Main.purs
   └── spago.dhall
   ```

   We'll write our main application code in `src/Main.purs`.


### Basic UI Interaction

Create `src/Main.purs` with the following basic code:

```purescript
module Main where

import Prelude
import DOM
import DOM.HTML
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Starting application"
  -- Add your DOM interaction code here
```

This sets up a basic entry point.  Further DOM manipulation (e.g., adding elements, setting text content) will be done in the next step.


### Data Fetching and Display

Now, let's fetch data and display it.  We'll use a placeholder API for simplicity. Replace the comment in `src/Main.purs` with the following:

```purescript
import Data.Maybe (Maybe(..), fromMaybe)
import Network.HTTP (runRequest, Request, get, responseBody)
import Data.Json (Json, decodeString)
import Data.Either (Either(..))


type APIResponse = { message :: String }

fetchAndDisplay :: forall effect. Aff effect (Either String APIResponse)
fetchAndDisplay = do
  let request = get "https://httpbin.org/get"
  response <- runRequest request
  responseBody response >>= decodeString >>= \case
    Right result -> return (Right result)
    Left err -> return (Left (show err))

main :: Effect Unit
main = do
  log "Starting application"
  fetchAndDisplay >>= \case
    Right {message} -> putStrLn $ "API Message: " <> message
    Left error -> putStrLn $ "Error: " <> error
```

This code uses `purescript-aff`, `purescript-node`, and `purescript-result` to fetch data from `https://httpbin.org/get` and handle potential errors.  The response is decoded as a JSON object using `purescript-json-codec` and then displayed using `log`. You'll likely need to add the necessary imports for this to work.  This would require defining the `APIResponse` type as needed, including handling different potential return types and error scenarios.


### Testing Your Application

To add testing, install `purescript-test`:

```bash
spago install purescript-test
```

Then, create a test file (e.g., `src/Test.purs`) and write tests for your functions.  This example is highly simplified; proper testing requires more comprehensive coverage.  Spago will run these tests using `spago test`.


Remember to compile and run your application using `spago build` and the appropriate command for your chosen runtime environment (e.g., `spago serve` for a web application or a command to execute the Node.js file for a backend application).  This example requires adjustments based on the specific packages used and the complexities of error handling, JSON decoding, and other factors.  Consult the documentation for each package used.


## Conclusion and Next Steps

This beginner's guide has introduced you to the fundamentals of PureScript, a powerful and elegant functional programming language.  You've learned about its core concepts, its ecosystem, and how to build a simple application.  Now it's time to take your skills further.


### Where to Go From Here?

This guide serves as a starting point. To truly master PureScript, you should focus on these next steps:

* **Build More Applications:** The best way to learn is by doing.  Start with small projects and gradually increase complexity. Try building a simple to-do list application, a basic game, or a more sophisticated web application integrating with external APIs.

* **Deepen Your Understanding of Functional Programming:** PureScript's strength lies in its functional paradigm.  Explore concepts like higher-order functions, functors, applicatives, and monads more deeply.  Understanding these concepts will unlock the true power and elegance of PureScript.

* **Explore the PureScript Ecosystem:** Familiarize yourself with the vast collection of PureScript libraries and packages.  Discover libraries that can help you with common tasks, such as working with the DOM, handling asynchronous operations, or integrating with external services.

* **Master Type Classes:** Type classes are a cornerstone of PureScript's type system.  Understanding how to define and use type classes will significantly improve your ability to write reusable and generic code.

* **Dive into Advanced Concepts:** Explore more advanced topics like effect systems, type-level programming, and optimizing your PureScript code for performance.


### Resources for Further Learning

Several excellent resources can help you continue your PureScript journey:

* **The PureScript Documentation:** The official PureScript documentation is a comprehensive resource covering various aspects of the language and its ecosystem.

* **PureScript by Example:** This online resource provides practical examples and explanations of various PureScript concepts.

* **The PureScript Discourse Forum:**  This online forum is a great place to ask questions, share your experiences, and engage with the PureScript community.

* **Online Tutorials and Courses:**  Search for PureScript tutorials and courses on platforms like YouTube, Udemy, and Coursera.

* **Books:** While there aren't many dedicated books on PureScript, books on functional programming concepts and Haskell can provide valuable insights, as PureScript borrows many ideas from Haskell.


### Community Engagement

The PureScript community is welcoming and supportive.  Engage with the community through the following channels:

* **The PureScript Discourse Forum:** Participate in discussions, answer questions, and share your knowledge.

* **GitHub:** Contribute to PureScript projects, report bugs, and propose improvements.

* **PureScript Meetups (if available in your area):**  Attend local meetups to network with other PureScript developers.

* **Online Communities:**  Engage with other PureScript developers on platforms like Reddit or Discord (check for relevant PureScript communities).

By actively engaging with the community, you can accelerate your learning, find solutions to your challenges, and contribute to the growth of the PureScript ecosystem. Remember, consistent practice and community involvement are key to becoming a proficient PureScript developer.

