# A Little Chat About Asserts

On November 19, 2024, over 2 million websites went down simultaneously.
Not because of a hacker attack.
Not because of server overload.
But because of a single `assert!()` in Cloudflare's Rust code.

There were tons of discussions.
But, as is often the case, most of them quickly turned into a holy war.
Lost in these battles was the main point: an interesting technical question about how different programming languages approach asserts.
This is what inspired me to write this article.

## Introduction

So what are these asserts anyway?
They are present in almost every programming language.
In C, it's `assert(condition)`; in Python, also `assert condition`; in Java, `assert condition : message`.
Rust offers a whole range of options: `assert!()`, `debug_assert!()`, `assert_eq!()`, `assert_ne!()`.
C# has `Debug.Assert()` and `Trace.Assert()`.
Ada offers `pragma Assert`.
Even JavaScript, which went without them in the standard for a long time, got them through the Node.js `assert` module.

Each programming language has its own peculiarities in the implementation and behavior of asserts.
However, to avoid confusion and focus on common concepts, we need to formulate a universal definition that will allow us to discuss them regardless of the specific language.

An **assert** (an assertion) is a statement that postulates a condition that *must always be true* at a certain point in the program's execution.
If the condition turns out to be false, it indicates a logical error or a bug in the code.
This is exactly the situation that should never happen if the code is written correctly.

The main purpose of asserts is to help find errors during development and testing.
They scream at us: "Stop! Something went wrong here! Your assumptions are wrong!"
It is a tool for detecting bugs in the early stages when they are still easy to fix.

## What Kinds of Asserts Are There?

The simplest version that everyone has seen is the usual `assert(condition)`.
We declare a condition that must be met.
Simple and clear.

But in the context of our definition, assertions are not limited to this.
In Rust, for example, there are `unwrap()` and `expect()`.
These methods contain an assert as part of their logic.

Assertions also include **pre-conditions** and **post-conditions**.
These are conditions that must be met before and after a function call.
Ada allows you to describe them directly in the function signature:

```ada
function Find_Max (A : My_Array_Type) return Integer with
  Pre  => A'Length > 0,
  Post => (for all J in A'Range => Find_Max'Result >= A(J)) and
          (for some J in A'Range => Find_Max'Result = A(J));
```

Here `Pre => A'Length > 0` is an assertion that will be checked before calling the function: the array cannot be empty.
The post-condition is more complex and even contains quantifiers, which can be thought of as verification loops.
The first line ensures that the result is not less than any of the elements.
The second line ensures that the result is equal to some element of the array.
Such a contract leaves almost no room for an incorrect implementation of the function.

There is an even more exotic option: **class invariants**.
These are assertions that must always be true whenever the object is in a "stable" state.
It's syntactic sugar to avoid inserting the same assert at the beginning and end of each method.
In Eiffel, this was a central concept of Design by Contract:

```eiffel
class sorted_pair
feature
  first, second: integer

  set(a, b: integer)
    local
      temp: integer
    do
      first := a
      second := b
      -- the invariant might be violated here!
      if first > second then
        temp := first
        first := second
        second := temp
      end
    end

invariant
  first <= second
end
```

The invariant `first <= second` is automatically checked at the entry and exit of every public method.
Inside the `set` method, it can be temporarily violated because at the beginning, we just assign values from arbitrary arguments.
But upon exiting the method, the invariant is guaranteed to be restored.
Yes, this was not strictly necessary, but we did it more to demonstrate the idea.

Similar to class invariants, there are also **loop invariants**.
This is an assertion that must be true at the beginning and end of each loop iteration.

## Behavior of Asserts

The behavior of asserts during development is more or less clear.
Their main goal is to help us, the developers.
When an assertion is false, the program should stop immediately and provide maximum information (file, line, condition, stack) so that the developer can quickly understand what went wrong.
It is a faithful assistant that sounds the alarm at the first sign of a problem.

But what should be done when the code gets into production?
When "the situation that should never have happened" suddenly occurs on a live server?
This is where a choice appears.
Let's see what the main options are.

### Option 1: Ignore the problem and hope for the best

The very first and probably simplest approach is to simply disable all checks in the release version of the code.
Many programming languages, like C++, do this by default.
The logic here is simple: asserts are for development, and in production, they only slow things down.
Once upon a time, in the age of dinosaurs and slow processors, this was a weighty argument.
Today, it no longer sounds so convincing.

What happens when such an assert would have fired, but it's not there?
The program finds itself in a state for which it was not designed.
This is a very slippery slope that leads straight to the infamous "Undefined Behavior" (UB).
The program may silently produce incorrect results, corrupt data, or crash much later, when finding the cause will be almost impossible.

But is this always bad?
Sometimes, weird behavior is better than a crash.
Imagine a computer game.
Let's take the good old Jagged Alliance 2.
There, character stats ranged from 0 to 100.
But due to various bonuses and modifiers, a character's stealth could exceed 127.
This led to a `char` overflow, and our ninja suddenly became more conspicuous than a herd of buffalo.
It's funny, it's a bug, but it's better than if the game just crashed at the end of a long battle you hadn't saved.
Sometimes a glitch on the screen is better than a complete program stop.
The user might not notice the strange behavior, but everyone will notice a crash.
Besides, our world would be a little sadder without glitches and speedruns.

### Option 2: Always crash (Rust-style)

The complete opposite of ignoring is the merciless crashing of the program immediately.
This is a kind of philosophy that Rust actively preaches.
The logic here is as follows: if we have reached a state that is impossible according to our assumptions, it means that the program is in an incorrect state.
Continuing to work in such a state can lead to unpredictable consequences, i.e., the same "Undefined Behavior".
It's better to crash right away than to rack your brains over the causes of strange errors later.

This is exactly what happened on November 19, 2024, with Cloudflare.
The assert triggered, the daemon crashed, and with it, 2+ million websites.
Technically, everything was correct: the system detected an invalid state and stopped, instead of taking risks.
But for the end-user, this resembles a sad joke: "Did the patient sweat before he died?".
From the user's point of view, the result is the same: the program doesn't work.

### Option 3: The "Golden Mean" — substituting logic

Between the two extremes — ignoring and crashing — there are a number of intermediate options.
Instead of a classic assert, which either disappears or brings everything down, we can "substitute" some other logic for the erroneous situation.
Often, since the days of Delphi, this has been implemented through an exception mechanism.
Sometimes you can see custom macros, like `ASSERT_WITH_CODE`, that execute certain code when triggered.
In essence, this can be seen as an admission that asserts in production are undesirable, and any potential error is better handled explicitly.

At first glance, this may seem like the perfect "golden mean."
The program doesn't crash but continues to work, trying to react correctly to an unexpected situation.
However, it has its own hidden pitfalls.
We have to write error handlers for situations that, according to our assumptions, should never occur.
Such code is often difficult to test.
To achieve 100% test coverage, you have to invent complex "mocks" for asserts or resort to other tricks.
Sometimes, in case of an error, you need to non-trivially roll back all the changes that have already been made.

And the worst part is the psychological aspect.
When you write a handler for an assert, you get a clear feeling that you are wasting your time because you are handling a situation that, by all logic, cannot exist at all.
This can be a bit demotivating.

Usually, in such scenarios, we "propagate" the error up the stack to be handled by a higher level of the program.
But it also happens that the error simply doesn't fit into the business logic.
For example, when authenticating a user, we need a binary answer: "allowed" or "not allowed."
Any answer in case of an error will be bad.
If we answer "no," it looks like a broken system to the user.
If we answer "yes," it's even worse: the system is broken from a security point of view.

## Option 4: Mathematics at your service

It seems we have gone through the main behavior options: ignore, crash, or try to handle the error.
All that remains is to choose what will work best in a particular case, depending on the specific task and project philosophy, and to accept the compromises.
But there is another, completely different way: one can prove that the assert will never trigger!

This approach sounds like something out of science fiction, but it is quite real.
The idea is not to think about what will happen *when* the assert triggers, but to mathematically prove that it will *never* trigger.
In this case, the runtime check becomes redundant, as we have an ironclad guarantee of its truth.

This is, so to speak, the holy grail of reliability.
Moreover, there are even several ways to achieve this goal, which are actively used in critical systems.

### SMT-solvers: automatic mathematicians

One way is to delegate the work to special tools called SMT-solvers (Satisfiability Modulo Theories).
In simple terms, this is a smart program that can automatically prove or disprove mathematical statements.
A prime example of this approach is the Ada language along with the SPARK tool.

Remember the Ada code example with pre- and post-conditions for the `Find_Max` function?
SPARK works like this: it feeds the SMT-solver all the pre-conditions of your function as initial facts.
Then it analyzes the code line by line, adding new facts.
For example, if you have the fact that `X > 5`, and the code says `Y := X + 1`, the solver concludes that `Y > 6`.
When SPARK encounters `assert(Y > 0)`, it tries to prove its truth based on the already known facts.
If it succeeds, the check is considered mathematically proven, so it can be safely thrown out of the runtime.

Sounds cool, right?
But there are nuances.
First, an SMT-solver is not all-powerful.
There is a whole class of problems that it cannot prove automatically.
Second, to help the solver, the programmer often has to add intermediate `assert`s that suggest the right line of thought.
The SMT-solver failed?
Let's add another assert.
Failed again?
Another assert.
Again?
We need to think...

This requires the developer to have not only programming skills but also a specific mathematical way of thinking and an understanding of how the solver works under the hood and what fact it is missing for a complete proof.
So it's a very powerful tool, but a complex one.
Definitely not a magic wand.

### Dependent types: a mathematician unto yourself

If SMT-solvers are complex, then hold on tight, because we're moving on to the heavy artillery.
These are programming languages with dependent types, such as Idris, Agda, or Coq.
They integrate mathematical proofs directly into the type system.

In a regular language, we can write the type "array of integers."
In a language with dependent types, you can write "array of exactly 10 integers."
You can do that in C++ too!
But let's go further: "array of length N," where N is a variable or a function parameter.
Or even "sorted array of integers."

The type becomes a mathematical statement; this is the well-known Curry-Howard isomorphism.
The program contains proofs of these statements.
If the compiler accepts the code, it means the proof is correct.

Of course, there is a "small" nuance: you will have to build this proof yourself.
While an SMT-solver creates proofs automatically (you write the code, it tries to prove it), here it's the other way around: you have to build the proof yourself.
The compiler only checks if the proof is correct.

And this is, to put it mildly, not easy.
It requires the programmer to be not just an engineer but also a little bit of a mathematician.
And that, you must agree, is a rather rare and highly paid skill.

If you're intrigued by what this might look like in code, read the next section.
If not — fast-forward to the [conclusions about Cloudflare](#conclusions-back-to-cloudflare), where everything is summed up.

Let's look at a small example in the Agda language.
Suppose we want to write a function that takes two non-empty arrays (vectors in C++ or lists in Python), concatenates them, and returns the second element of the result.

```agda
-- Takes two non-empty vectors and returns the second element of their concatenation
second : {A : Set} {n m : ℕ} → 𝕍 A (suc n) → 𝕍 A (suc m) → A
second {A} {n} {m} vec1 vec2 =
  let one = suc zero
      concatenated : 𝕍 A (suc n + suc m)
      concatenated = vec1 ++ vec2
      -- Proof that index 1 is in bounds
      index-proof : one < suc n + suc m
      index-proof = 1<suc-n+suc-m
  in nth concatenated one index-proof
```

What do we see?
The `second` function takes as many as five arguments.
The first three — `{A : Set} {n m : ℕ}` — are written in curly braces.
These are implicit arguments, kind of like default parameters.
Only, unlike in most programming languages, their values are not set by the programmer but **inferred by the compiler**.

`A` is an argument of type `Set`, which contains types.
For the pedantic, I'll explain that Set contains first-order types because the type of all types leads to Russell's paradox.
That is, `A` specifies the type of the array elements (whether they are numbers, strings, or something else).

`n` and `m` are natural numbers, the lengths of the arrays, or rather, length minus one, because the real length is `suc n` and `suc m`.

And then there are two arguments that we need to specify explicitly.
The first is `𝕍 A (suc n)` — an array of length `n + 1`.
The second is `𝕍 A (suc m)` — an array of length `m + 1`.
Here `suc` means the next natural number.
Since `n` and `m` are natural numbers (and can be zero), the lengths of the arrays are at least 1.
This means that neither array can be empty, because the minimum length is always 1.
The compiler will not allow you to pass an empty array — it will be a type error.

When you call `second vec1 vec2`, the compiler looks at the types of `vec1` and `vec2` and figures it out for itself:
Aha, `A` is the type `Int` (a built-in type for integers, not to be confused with ℕ — the mathematical type of natural numbers that exist at compile time), `n = 5`, `m = 3`.
It's similar to generics in Java or templates in C++, only much more powerful.

The body of the `second` function consists of two actions: concatenating the arrays (nothing to check, we can always do this) and getting the second element, which has an index of 1.
For this, we call the `nth` function, and here's the interesting part:

```agda
nth : {A : Set} {n : ℕ} → 𝕍 A n → (i : ℕ) → i < n → A
nth []       i       ()
nth (x ∷ xs) zero    (z<s)     = x
nth (x ∷ xs) (suc i) (s<s i<n) = nth xs i i<n
```

Let's look at the signature of this function.
It also has five arguments!
First `A` — the type of the array elements.
Then `n` — the length of the array.
Then the array itself.
Then the index.
The last argument has the type `i < n`, which means it must contain a proof that `i` is always less than the length of the array!
And we pass the lemma `1<suc-n+suc-m` there, and yes, that's an identifier with less-than, plus, and minus symbols!

Again, let's think about the indices.
To get the second element, we need to pass a proof that the length of the array is greater than one.
The first array has length `n+1`, the second has `m+1`, so the concatenation has length `n+m+2`.
Intuitively, it is obvious that the length of the `concatenated` array is greater than one.
But... Agda requires a formal proof of this fact!
We'll include it here to intimidate the reader a little bit:

```agda
subst : {A : Set} {x y : A} (P : A → Set) → x ≡ y → P x → P y
subst P refl px = px

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

+-suc : (n m : ℕ) → n + suc m ≡ suc (n + m)
+-suc zero    m = refl
+-suc (suc n) m = subst (λ k → suc n + suc m ≡ suc k) (+-suc n m) refl

1<suc-n+suc-m : {n m : ℕ} → suc zero < suc n + suc m
1<suc-n+suc-m {n} {m} = s<s (subst (λ k → zero < k) (sym (+-suc n m)) z<s)
```

It is not our task to analyze step-by-step how these proofs are constructed, because that is a separate, large topic.
The main thing is: if the proof is incorrect — the code will not compile.
If the proof is correct, then the `nth` and `second` functions are **guaranteed** not to fail with an "index out of bounds" error.
The language's type system excludes this error.

Beautiful?
Absolutely.
Difficult?
You bet.

Look at the code: half of it is not business logic but proof construction.
The lemmas `+-suc` and `1<suc-n+suc-m` are mathematical constructs.
To write such code, you need to understand constructive mathematics, type theory, and have the patience of a saint.

## Conclusions: Back to Cloudflare

So, we've come a long way from simple asserts to dependent types.
Now let's go back to where we started: the Cloudflare incident.
Could it have been avoided, and what would the ideal solution look like?

In my opinion, the key to solving this problem lies in a clear separation of two worlds:
The dangerous external world and the safe internal world.
We haven't touched on this topic, but it's the IO monad.
If a function returns this data type, it means it works with the outside world: it reads a configuration file, works with a database, makes a network request.
Here, absolutely everything can go wrong: the file is corrupted, the database is unavailable, the JSON has the wrong fields, so such a function has to be paranoid.

The IO monad marks such code.
It hints that this is a place where you should return `Maybe Config`.
This is Haskell notation; for others, `Option<Config>` might be clearer.

Separately, there is a function for checking access.
This is pure logic, we work only with guaranteed correct data, and `assert` becomes our friend again.
It is a signal that we have overlooked something in our algorithms.
And in an ideal world, proven with SPARK or Agda, their truth is proven mathematically.

To separate these two functions, the types force us to build the right architecture:
we read or update the config.
If we get an error, we either don't start the daemon, or we log the error and don't touch the old config.
As a result, either the server will not start, which we will see immediately, or if it does start, it will no longer crash because of the config.
