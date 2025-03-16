+++
title = "Beginner's Guide to Agda"
date = 2024-12-19
toc = true
readTime = true
+++

## Introduction to Agda

### What is Agda?

Agda is a dependently typed programming language and proof assistant.  This means it combines the features of a functional programming language (like Haskell or ML) with the ability to formally verify the correctness of your programs.  You write programs in Agda, and the compiler helps you ensure that those programs meet their specifications. This is done through a rich type system that allows you to express and enforce complex relationships between data and functions.  Unlike many programming languages where types are primarily for the compiler's benefit, in Agda, types are a crucial part of the programming process, acting as specifications and documentation.  Correctly-typed Agda code is guaranteed to be correct according to its type signature.

### Why use Agda?

Agda is particularly useful for:

* **Formal verification:**  Ensuring your programs are correct by mathematically proving their properties.  This is crucial in domains like safety-critical systems or cryptography.
* **Learning about type theory:** Agda provides a hands-on way to learn and explore concepts from type theory, a foundational area of computer science and logic.
* **Writing highly reliable code:** The strong type system helps prevent common programming errors at compile time.
* **Developing robust and well-documented programs:** Type signatures act as precise specifications, making your code easier to understand and maintain.
* **Exploring advanced programming paradigms:** Agda supports concepts like dependent types, inductive types, and higher-kinded types, enabling elegant and powerful solutions to complex problems.


### Setting up your environment

To start using Agda, you'll need to install the Agda compiler and a suitable editor or IDE.  The most straightforward approach is to use the Agda distribution available on the official website ([link to Agda website]).  This typically includes the compiler and a package manager.  

For editing, several options exist:

* **Emacs with `agda-mode`:** A powerful and widely used combination.  `agda-mode` provides syntax highlighting, autocompletion, and convenient interaction with the Agda compiler.
* **VS Code with the `agda` extension:** A more modern and user-friendly IDE option that offers similar features to `agda-mode`.
* **Other IDEs:** While less common, other IDEs may offer limited support for Agda.

Once Agda is installed and you've chosen your editor, you can create and run Agda files (typically with a `.agda` extension).  Consult your editor's documentation for instructions on setting up Agda support.

### Your first Agda program

Let's write a simple Agda program that adds two numbers:

```agda
module MyFirstProgram where

open import Data.Nat

add : ℕ → ℕ → ℕ
add zero y = y
add (suc x) y = suc (add x y)

main : IO ()
main = putStrLn "Hello, Agda!"
```

This code defines a function `add` that recursively adds two natural numbers (`ℕ`). The `zero` and `suc` constructors represent zero and the successor function respectively.  The `main` function demonstrates simple output to the console.

To compile and run this program, you will need to save it to a file (e.g., `MyFirstProgram.agda`) and use the Agda compiler from your terminal.  The exact commands will depend on your environment and Agda setup.  Consult the Agda documentation for details on compiling and running your programs. Note that running the `main` function might require an additional step depending on your setup. This example requires the `IO` monad, which you might need to import from a suitable library.  The details of IO handling are beyond the scope of this introductory section.


## Basic Syntax and Types

### Data types: defining your own types

Agda allows you to define your own data types using inductive definitions.  This means you specify the constructors that build values of the type.  A simple example is defining a type for boolean values:

```agda
data Bool : Set where
  true  : Bool
  false : Bool
```

This defines a type `Bool` which has two constructors: `true` and `false`.  The `Set` indicates that `Bool` is a type inhabiting the universe of sets (a collection of types).

More complex types can be defined similarly. For instance, a list type:

```agda
data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A
```

This defines `List A`, a list of elements of type `A`.  `[]` represents the empty list, and `_∷_` (pronounced "cons") adds an element to the beginning of a list.  The underscore `_` indicates an infix operator.

### Functions: defining functions

Functions in Agda are defined using pattern matching on the input arguments.  Let's define a function to check if a boolean value is true:

```agda
isTrue : Bool → Bool
isTrue true  = true
isTrue false = false
```

This function `isTrue` takes a `Bool` as input and returns `true` if the input is `true`, and `false` otherwise.  The definition uses pattern matching: the first line handles the case where the input is `true`, and the second line handles the case where the input is `false`.


Another example, a function that computes the length of a list:

```agda
length : {A : Set} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs)
```

This uses the natural number type `ℕ` (often imported from `Data.Nat`).  `zero` represents 0 and `suc` is the successor function (adds 1).

### Type signatures

It's crucial to provide type signatures for your functions and data types in Agda.  Type signatures specify the expected input and output types, allowing Agda to perform type checking and ensuring correctness. The examples above already show type signatures. For instance, in `isTrue : Bool → Bool`, `Bool → Bool` states that `isTrue` takes a `Bool` as input and returns a `Bool`.

### Pattern matching

Pattern matching is a powerful feature in Agda. It allows you to define functions by specifying how they behave for different possible inputs. The examples above already show pattern matching. In the `length` function, we use `[]` to match the empty list and `x ∷ xs` to match a non-empty list with head `x` and tail `xs`.  Agda guarantees that all possible cases are covered (exhaustiveness) and that each case is handled uniquely (non-ambiguity).  If you don't cover all cases, the compiler will give an error.

### Basic operators

Agda supports several basic operators, including arithmetic operators (`+`, `-`, `*`, `/`), logical operators (`∧`, `∨`, `¬`), and equality operators (`≡`, `≠`).  These operators are typically defined within modules that are imported, like `Data.Nat` for natural number arithmetic. You can define your own operators as well using infix notation (as seen with the `_∷_` operator in the `List` example).  The specific behavior of these operators will depend on the types they operate on (e.g., addition on natural numbers, boolean conjunction).  Be sure to consult the Agda standard library documentation for the precise definitions and usage of these operators.


## Dependent Types

### Understanding dependent types

Dependent types are types that depend on values. This means that the type of a value can be determined by the value of another expression.  This is a powerful feature that distinguishes Agda from simply-typed languages.  The most common way to encounter dependent types is through dependent function types and dependent pairs.

A simple example is a function that returns a vector of a specific size:

```agda
open import Data.Vec
open import Data.Nat

-- A vector of type A with length n
Vec : ℕ → Set → Set
Vec zero A = ⊤  -- Empty type (only one inhabitant)
Vec (suc n) A = A × Vec n A

myVec : Vec 3 ℕ
myVec = (1 , (2 , (3 , [])))
```

Here, the type `Vec n A` is a dependent type; its structure depends on the value of `n`.  If `n` is 0, it's the empty type; otherwise it's a pair where the first element is of type A and the rest is a vector of `n-1` elements of type A.


### Vectors

The `Data.Vec` module provides a definition of vectors, which are lists of a fixed length. The length of the vector is part of its type.  This allows you to write functions that are only valid for vectors of a specific length, enhancing type safety. For example, a function to access the element at a specific index would only be defined for indices within the bounds of the vector's length.

Example:

```agda
open import Data.Vec

get : ∀ {n} {A : Set} → Vec n A → ℕ → A
get (x ∷ xs) zero = x
get (x ∷ xs) (suc n) = get xs n
```

This `get` function is only defined for indices less than or equal to the length of the vector (due to the pattern matching).


### Lists

While lists in Agda are not inherently dependent types in the same way vectors are, they can still participate in dependent type constructions.  The key difference is that the length of a list isn't statically known like with vectors; it can vary at runtime.  This means you'd typically use dependent pairs or other techniques to associate a length with a list if needed.

Example showing dependent pairs with lists:

```agda
open import Data.Nat
open import Data.List

Pair : Set → Set → Set
Pair A B = Σ A B

listLength : {A : Set} → List A → ℕ
listLength [] = 0
listLength (x ∷ xs) = suc (listLength xs)

myListWithLength : Pair (List ℕ) ℕ
myListWithLength = (1 ∷ 2 ∷ 3 ∷ [], listLength (1 ∷ 2 ∷ 3 ∷ []))
```

Here, `Pair` creates a dependent pair where the second component's type is a natural number representing the length of the list.


### Working with dependent pairs

Dependent pairs (Σ types) are a crucial aspect of working with dependent types.  A dependent pair `Σ A B` consists of a value of type `A` and a value of type `B`, where `B` can depend on the value of `A`. The syntax to construct a dependent pair is `(a , b)` where `a` has type `A` and `b` has type `B a`. To access the components of a dependent pair, you can use pattern matching:

Example:

```agda
open import Data.Nat
open import Relation.Binary.PropositionalEquality

data MyType : Set where
  MkMyType : ℕ → MyType

myFunction : (p : Σ MyType (λ x → ℕ)) → ℕ
myFunction (MkMyType n , m) = n + m
```

This `myFunction` takes a dependent pair as an argument. The type of the second element of the pair (`m`) depends on the value of the first element (`MkMyType n`). The pattern matching deconstructs the pair to access and use its components.




## Advanced Concepts

### Inductive types and recursion

Inductive types, as seen earlier, are fundamental to Agda.  They allow defining types recursively, building complex structures from simpler ones.  Recursion is then used to define functions that operate on these types.  However, Agda's type system enforces strong constraints on recursion to guarantee termination.  This is done through *guarded recursion*.

Consider a function to compute the factorial:

```agda
open import Data.Nat

factorial : ℕ → ℕ
factorial zero = 1
factorial (suc n) = suc n * factorial n
```

This looks like a standard recursive definition, but it's actually not directly accepted by Agda without further specification.  Agda needs to ensure this recursion terminates.  A way to express this is using pattern matching on the argument, which provides a well-founded order for recursion.


A more complex example demonstrating guarded recursion could involve mutual recursion between two functions that need careful termination analysis.  This might necessitate the use of more explicit termination proofs.


### Type classes

Type classes provide a way to define generic functions that operate on different types, providing a form of polymorphism.  In Agda, type classes are implemented using records with associated functions.

Example:

```agda
open import Data.Maybe

-- Type class for equality
record Eq (A : Set) : Set where
  field
    _≡_ : A → A → Bool
    refl : ∀ x → x ≡ x

-- Instance for Bool
Eq-Bool : Eq Bool
Eq-Bool = record { _≡_ = λ x y → if x then y else ¬ y ; refl = λ x → true }

testEqBool : true ≡ true
testEqBool = refl true

```

This defines an `Eq` type class with an equality function `_≡_` and reflexivity property `refl`.  Then an instance `Eq-Bool` is defined specifically for the `Bool` type.



### Records

Records are similar to structs or objects in other programming languages, allowing you to group together values of different types under a single name.  They can also incorporate dependent types, where the type of one field can depend on the value of another.

Example:

```agda
record Point (dim : ℕ) : Set where
  constructor MkPoint
  field
    coords : Vec dim ℝ

myPoint : Point 2
myPoint = MkPoint (1.0 ∷ 2.0 ∷ []) -- Assuming ℝ is defined and a suitable Vec instance exists
```

Here, `Point` is a record where the dimension (`dim`) is part of its type, and it has a field `coords` representing the coordinates as a vector of the specified dimension.



### Modules and imports

Agda uses modules to organize code into reusable units.  Modules are files with a `.agda` extension.  You can import modules using the `open import` or `import` keywords.  `open import` imports all names from the module into the current scope; `import` requires explicit qualification with the module name.

Example:

```agda
open import Data.Nat  -- Imports everything from Data.Nat
import Data.List -- Requires explicit qualification, e.g., Data.List.length
```

Proper modularization is essential for managing large Agda projects.  Using modules effectively enhances code reusability, maintainability and readability.  It avoids namespace collisions by defining separate contexts for different functionalities.



## Practical Applications and Examples

### Simple Example: Implementing a function

Let's implement a function to check if a number is even or odd using pattern matching and natural numbers:

```agda
open import Data.Nat
open import Data.Bool

isEven : ℕ → Bool
isEven zero = true
isEven (suc zero) = false
isEven (suc (suc n)) = isEven n
```

This function `isEven` recursively checks for evenness. The base cases are `zero` (even) and `suc zero` (odd).  The recursive case handles the rest. Note how the pattern matching on the `suc` constructor ensures that we are always reducing the input towards the base cases, guaranteeing termination.


### Intermediate Example: Working with Vectors

This example demonstrates working with vectors and utilizes dependent types to ensure type safety:

```agda
open import Data.Vec
open import Data.Nat

-- Function to find the maximum element in a vector of natural numbers.
findMax : {n : ℕ} → Vec n ℕ → ℕ
findMax [] = 0  -- Handle empty vector case
findMax (x ∷ xs) = max x (findMax xs)

-- Helper function to find the maximum of two natural numbers.
max : ℕ → ℕ → ℕ
max x y = if x ≤ y then y else x

-- Example usage
exampleMax : findMax (1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ≡ 4
exampleMax = refl
```

The `findMax` function recursively compares elements to find the maximum.  The type `{n : ℕ} → Vec n ℕ` shows a dependent type; the function's behavior is linked to the vector's length.  The `exampleMax` demonstrates testing equality using Agda's `≡` operator and `refl` to prove reflexivity. This requires importing the `Relation.Binary.PropositionalEquality` module.


### Advanced Example: Implementing a data structure

Let's implement a binary search tree (BST) data structure:

```agda
open import Data.Nat
open import Data.Maybe
open import Data.List
data Tree (A : Set) : Set where
  node : A → Tree A → Tree A → Tree A
  empty : Tree A

-- Function to search for a value in the BST.  Returns Just a if found, Nothing otherwise.
search : {A : Set} → (A → A → Bool) → A → Tree A → Maybe A
search _ _ empty = nothing
search lt x (node y left right) =
  if lt x y then search lt x left
  else if x ≡ y then just y
  else search lt x right
```


This example defines a binary search tree (`Tree`). The `search` function uses the comparison function (`lt`) provided to recursively search for a specific value in the tree. The `≡` operator represents equality and `just` and `nothing` are constructors for the `Maybe` type.  Remember to import necessary modules for `Maybe`, comparison functions, and equality.  A complete implementation would include functions for insertion, deletion, and other tree operations.  The use of a comparison function (`lt`) makes the tree generic and capable of handling different ordered types.  Further enhancements might involve adding proofs of BST properties to ensure correctness.


## Further Learning and Resources

### Online resources and tutorials

The official Agda website ([link to Agda website]) is an excellent starting point.  It provides documentation on the language, its standard library, and links to other relevant resources.  Numerous online tutorials and blog posts cover various aspects of Agda programming.  Searching for "Agda tutorial" or "Agda programming" on sites like YouTube, Google, and other educational platforms will yield many results, ranging from beginner-friendly introductions to more advanced topics.  Many university courses also use Agda and their lecture notes or online materials might be publicly accessible.  Look for courses on type theory, functional programming, or formal verification that utilize Agda as their language of instruction.


### Community and forums

The Agda community is active and supportive. Engaging with the community is a valuable way to learn from experienced users, get help with problems, and contribute to the development of the language itself.  Several avenues exist to connect with the Agda community:

* **Agda Mailing List:** Subscribe to the Agda mailing list to participate in discussions and ask questions.
* **Agda Discourse Forum:** Check the Agda Discourse forum for discussions, questions, and announcements.  This is a newer, and potentially more active, platform than the mailing list.
* **Stack Overflow:** Search for Agda-related questions on Stack Overflow and contribute your knowledge when possible.
* **GitHub:** Agda's source code and many related projects are hosted on GitHub.  Contributing to these projects or participating in issue discussions can provide valuable learning experiences.


### Books and articles

While there isn't a large volume of books specifically dedicated to Agda, several resources offer comprehensive treatments of related topics:

* **"Certified Programming with Dependent Types" by Benjamin C. Pierce et al.:** This book provides a thorough introduction to dependent types and their application in programming. While not exclusively focused on Agda, it covers many relevant concepts.
* **"Software Foundations" by Benjamin C. Pierce et al.:**  This series of textbooks introduces foundational concepts of computer science through Coq, a proof assistant similar to Agda. The concepts learned are largely transferable to Agda.
* **Research papers:**  Numerous research papers explore advanced topics within the Agda language and the broader field of dependent type theory. These can be a valuable resource for those looking to deepen their understanding of the language and its applications in advanced programming paradigms and formal methods.  Searching academic databases like ACM Digital Library, IEEE Xplore, and Google Scholar for terms such as "Agda," "dependent types," and "proof assistants" will yield many results.


Remember to always check the publication date of any online or printed material to ensure it is up-to-date with the latest features and changes in the Agda language.

