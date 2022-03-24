# Design and Implementation of the reactive-banana FRP library

by Heinrich Apfelmus,
April 2022

licensed CC-BY-SA 4.0

DRAFT, INCOMPLETE

This document is intended to describe the design and implementation of the reactive-banana library for functional reactive programming (FRP).

NOTE: This draft text already uses the plural `Events` as opposed to the singular `Event` for the data type.

# Introduction

Functional reactive programming (FRP) is a programming paradigm for declarative programming with data that changes over time. It was introduced in the seminal paper [Elliot and Hudak (1997)][fran] with two concepts, *Behavior* and *Events*. A *Behavior* denotes a value that varies in time, i.e. *Behavior* is a data type that represents the entire history and future of a value. *Events* denote a sequence of discrete moments in time which carry additional data.

For example, consider a graphical user interface (GUI) with a button and a numerical display whose value tracks the number of times that the button has been pressed. This value changes over time and can be represented as a *Behavior*. The clicks on the button are represented as *Events*; specifically, we have a variable of type `Events ()` that represents all future (!) clicks. The purpose of our program is to specify how these button `Events ()` are translated into a counter `Behavior Int` in a declarative manner. Of course, it is not possible to know all future clicks and write them into computer memory in advance, but it is useful to *pretend* to know them in advance, and to write a declarative program with that. As long as this program does not peek into the future, i.e. as long as it does not violate causality, it can be executed. The purpose of the reactive-banana FRP library is to execute such programs.

  [fran]: http://conal.net/papers/icfp97/

[…]

# Semantics

* Behavior uses **continuous time**. There is no notion of "update" for a Behavior. (For reasons of efficiency, the implementation does keep track of updates, though.) The advantage of this design is that it is not possible to write programs that depend on how often a Behavior updates, especially for updates that do not actually change the value.

* **Recursion** is allowed (and necessary). Specifically, all mututal recursions between Behavior and Event are well-defined.

[…]

# Implementation

## Time leaks and Updates

The easiest way to implement Events would be as a list of values tagged with their time of occurrence, e.g.

    type Events a = [(Time, a)]

By using a list lazy, future events which are not yet known can be added to the list when they occur.

However, any representation of Events as a pure data structure is problematic: As long as a variable `x :: Events A` is in scope, purity of the data structure implies that all events that have ever occurred will be kept alive as long as the variable `x` is alive. This is known as a **time leak**: Past events are stored and have to be traversed (repeatedly even) in order to reach the present moment. In order to be efficient, we have to discard old events which are no longer relevant, and this implies that the `Events` data structure needs to support impure updates. Note that lazy lists already use impure updates through their laziness, though these are transparent at the language level. Also, the problem of time leaks becomes apparent mostly in the presence of dynamic event switching, when the first-class nature of `Events` is used extensively.

## Push- versus pull

[…]

The necessity of implementing `Events` as a data structure that supports updates is challenging. We split the problem into several parts:

1. *Low-level*. We implement a mutable graph which represents dependencies between Events and allows us to implement push-based propagation of information.

2. *Mid-level*. We define low-level data structure `Pulse` and `Latch` that roughly correspond to Events and Behavior, but which are essentially just references to nodes in the mutable graph above. New nodes can be created with monadic functions such as

        union :: Pluse a -> Pulse a -> Build (Pulse a)

    where `Build` is a monad that describes changes to the mutable graph.

3. *High-level*. We implement *Event* and *Behavior* in terms of `Pulse` and `Latch`. Here, we use **observable sharing** to map Haskell variables to mutable references, and to turn impure functions into pure functions where possible.

## Low-level: Mutable graph

* The difficulty of `union` in a push-based setting: Dependency order, priorities.
* `Vault` for storage.
* Garbage collection

[…]

## Mid-level: Pulse and Latch

* `Build` monad.

[…]

## High-level: Events and Behavior

[…]

### Events and Behaviors

The final step in the implementation of the `Events` and `Behavior` types is their definition in terms of the `Pulse` and `Latch` types. The implementation is very direct, roughly `Events ~ Pulse` and `Behavior ~ Latch`, except that

* we use *observable sharing* to make `Events` and `Behavior` behave more like pure values, and
* for each `Behavior`, we keep track of a its updates, so that `Behavior a ~ (Latch a, Pulse ())`, in order to more efficiently connect it to the outside world.

### Observable sharing

This section explains how observable sharing is used in the implementation of `Events` and `Behavior`.

*Observable sharing* is a technique that aids the implementation of *domain specific languages* (DSL) that are *embedded* in a purely functional host language. It makes the variable binding and sharing mechanism of the host language (e.g. Haskell's `let`) partially observable in the embedded language in order to avoid duplicating large subexpressions ([Claessen and Sands 1999][clsa], [Gill 2009][gill], [Kiselyov 2011][oleg],…). We use this technique for the same purpose in our implementation of FRP.

To explain observable sharing, let us first consider a toy example and only then consider an FRP example. The toy example is a small language for evaluating arithmetic expressions `Exp`:

    data Exp
        = Val Int     -- plain `Int`
        | Add Exp Exp -- addition
        | Mul Exp Exp -- multiplication

An interpreter for this language has type signature `eval :: Exp -> Int`, we skip the straightforward implementation here. To embed this language more deeply into the host language, we would also define lower-case (smart) constructors like `val = Val` and even a `Num Exp` instance with `(+) = Add` and `(*) = Mul`.

The host language acts a sort of "macro language" for the embedded language. For example, consider the value

    y :: Exp
    y = let x = val 1 + val 2 in (x * x)

This expression is equal to

    y = Mul (Add (Val 1) (Val 2)) (Add (Val 1) (Val 2))

In other words, the variable `x` behaves like a "abbreviation" or "macro" that expands to a full expression in the embedded language. Unfortunately, the resulting expression has become inefficient — the addition has been *duplicated*! What to do?

In order to have more *sharing* and to avoid this duplication, we have to recognize two things:

* The host language allows sharing expressions by binding them to variables, but the embedded language does not allow that yet. If we want sharing in the DSL, we have to add an explicit *language construct* for that. The simplest solution is to add variables and binders to the language, that is to expand the type with two constructors

        data Name = String
        data Exp
            = …
            | Var Name
            | Let Name Exp Exp -- let name = e1 in e2

    Then, the example value would be written

        y' = Let "x" (val 1 + val 2) (Var "x" * Var "x")

* It would be swell if we could reuse the host language `let` syntax for the embedded `Let` as well. But there is a conflict of interest here: Even if we have a way of expressing sharing in our embedded language (`Let`), we may still want to preserve our ability to define macros like `x` that are *not* shared, but instead duplicate code (`let`) — it is not obvious that host language expression `y` should always be translated to its shared variant `y'`. However, in most cases, this is indeed desirable, certainly in the case of our FRP implementation.

The main difficulty we face when attempting to reuse the `let x = …` construct of the host language is that this construct is *referentially transparent*, i.e. the semantics of the host language do not allow us to learn that a variable named `x` was defined. This is where *observable sharing* comes in. Its main idea is as clever as it is devious (another word for "not referentially transparent"): By making the evaluation of the "macro" `x` impure (using `unsafePerformIO`), we can detect (using a mutable reference) repeated uses of the "macro" and expand it to a variable instead of the full expression. In this way, we do not learn the name of the variable `x`, but we do retain the fact that it always refers to the same expression in the embedded language.

Let us leave aside observable sharing for a moment and focus on impurity: It turns out that *monads* lend additional insight into the distinction between a "macro" `x` and a "variable" `x`. Specifically, there are many similarities between the `let` construct and a monadic bind. To wit, the pure expression

```
let x = exp1 in
let y = exp2 in
exp3
```
very much resembles the monadic expression
```
do
    x <- exp1
    y <- exp2
    pure exp3
```
We can use this explicitly by introducing a monad, say `M`, which is (contains) a state monad that stores a collection of associations `Name ↦ Exp` of variable names and expressions assigned to them. Such a monad would be support the two operations
```
store :: Exp -> M Name
lookup :: Name -> M (Maybe Exp)
```
for creating variables and looking them up.
With such a monad, we can actually consider the type `M Name` to be a replacement for `Exp`, as we can directly embed every `Exp` into this type by using `store`, but we can also turn every `Name` back into an `Exp` by using `Var :: Name -> Exp`. In addition, this replacement now gives us a nice distinction on the type-level between a "macro" `M Name`, i.e. an action that adds a new expression to the store, and a "variable" `Name` which merely references a shared expression. Assuming that the functions `val`, `(+)` and `(*)` have been adapted to this type replacement, e.g. `val :: Int -> M Name`, we can now write the shared and unshared examples from above as
```
y :: M Name
y = do
    let x = val 1 + val 2
    x * x

y' :: M Name
y' = do
    x' <- val 1 + val 2
    let x = var x'
    x * x
```
where `var = pure . Var`. The code is almost identical, except that the "macro expansion" case `y` uses the host language `let` construct, while the "shared variable" case `y'` uses the monadic assignment `<-`, which essentially serves as a (variant of the) embedded language `Let`. Now, the purpose of *observable sharing* is to make it so that `y` generates the same expressions as `y'`, and the way to do that is to change the behavior of the "macro" `x :: M Name`: Instead of creating a new expression every time that this monadic action is executed, it creates an expression only once, stores the result in a variable `x'`, and returns `var x'` whenever it is executed (this can only be done using `unsafePerformIO`).

Reactive-banana […]


  [clsa]: http://www.cse.chalmers.se/~dave/papers/observable-sharing.pdf 
  [gill]: http://www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf
  [oleg]: https://arxiv.org/abs/1109.0784s