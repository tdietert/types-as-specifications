# Types as Specifications: An Introduction to Type-Level Programming in Haskell

## Author: Thomas Dietert

## Description

Haskell is known for being a pure, statically typed functional programming 
language that, if used correctly, allows you to write more correct software. 
In this talk you will learn how to leverage existing Haskell features and 
extensions to write more correct software through type-level programming.

Statically typed programming languages help us write more correct software by
allowing us to enforce customizable invariants about run time values at compile
time. As Leslie Lamport once said: “Everyone thinks they think, but if you don’t
write down your thoughts you are fooling yourself”. Writing specifications,
however informal, before writing code is crucial to the process of writing
correct software. Manual type annotations in statically typed languages are a
sort of specification. They force us to write down our thoughts before we code
by conveying to the compiler invariants about the way values should propagate
through our programs. The more specific we are with our specifications (types),
the more code invariants the compiler can enforce about out program before we
ever run it.

This talk will guide the attendee through the notion of types as specifications
and describe how to embed invariants about program runtime behavior into the
Haskell type system. Using a collection of language extensions introduced at a
steady interval, this talk will cover topics ranging from statically checking
the length of lists to defining and applying higher order type-level functions.
It will conclude with an example of embedding program behavior specifications as
type-level state machines through the culmination of all prior material
presented, in addition to indexed monads.

If you are an intermediate Haskell developer striving to understand how to bend
GHC and the Haskell type system to your will more effectively, or a beginner
Haskell tinkerer desiring to expand your knowledge of the benefits of modern
Haskell language extensions, then you should attend this talk!

## Overview

Introduction
------------
- Types as specifications
- Values, types, & kinds
- Type-level functions
- Value & Type Promotion
- Phantom Types (Tagging)

Type-level Programming
----------------------
- Type-level Naturals (Peano Encoding)
- Type-level Lists & Heterogenous Lists
- Type Operators 
- Constraint Kinds
- Correct by Construction

[[ Exercises ]]
- Type-level Natural Number Multiplication
- Type-level Booleans & Boolean Operators 
- Type-safe List Indexing
- Type-level List operations

Type-level Defunctionalization
------------------------------
- Value-level Defunctionalization
- Higher-order type families (`data Eval` & `type Exp`)
- Type-family composition (Type-level monads)

[[ Exercises ]]
- Type-level Natural Number Multiplication
- Type-level Booleans & Boolean Operators 
- Type-safe List Indexing
- Type-level List operations

Type-level State Machines
-------------------------
- Encoding states and transitions as types
- Type-signatures constrain implementation
- Indexed Monads
- Type-level state machine

Resources
---------
- `row-types` library documentation
