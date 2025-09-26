# Formal Verification for Mortals

When you ask how to make sure your program works correctly,
you might hear many answers like write unit tests,
hire more QA, use static analyzers,
or just pray that nothing breaks in production.
But often people forget, or don't even know,
that formal verification exists.
What is it?
It's when there's a mathematical proof
that your program works correctly according to a formal specification.
In an ideal world, this should make unit tests and QA... unnecessary,
at least for those parts of the system that are formally verified.
Is this really as cool as it sounds?

To understand this, let's first look at the hierarchy of verification methods.
The aviation standard DO-178C defines safety levels depending on code criticality:

| Level | Name | Failure Impact Description | Verification Requirements |
|-------|------|----------------------------|---------------------------|
| E | No Effect | No impact on aircraft safety | Testing is optional |
| D | Minor | Minor inconvenience to crew | Basic unit tests<br>Code review |
| C | Major | Serious problems but no threat to life | 100% code coverage<br>Integration tests<br>Manual QA |
| B | Hazardous | Possible injuries to passengers or crew | Static analysis<br>Property-based testing<br>Fuzzing |
| A | Catastrophic | Threat to life or catastrophe | **Formal verification**<br>Mathematical proof of correctness |

You can see that formal verification stands at the top of the pyramid, the highest level of code safety that can be achieved!
Whether you should use it in your projects is up to you,
my task is only to explain how it works.

There's a great resource called Software Foundations dedicated to formal methods for program verification.
It has Volume 5 dedicated specifically to formal verification of C code.
But it also says "before reading, you need to work through Volume 1
(and do all the exercises), then Volume 2 (and do all the exercises)".
Of course, not everyone has the time and inspiration to do this.
And the text itself is "open Coq, execute a command,
it works like this, but that command doesn't work".
So I decided to just take the first usage example from there,
something like Getting Started,
and retell its content at the "broadly speaking" level,
without diving into Coq magic.
Consider this entertainment content.

What is formal verification and what beast is this Coq?
Coq is a proof assistant, a tool for checking mathematical proofs.
Imagine a very pedantic editor
who checks every comma in your article
and won't let a single logical error pass.
Only this "editor" is a computer program
that knows everything about logic and mathematics.
Formal verification consists of
recording requirements as mathematical statements,
and then proving that our code meets these requirements.
But enough words, let's move to an example.

```c
#include <stddef.h>

unsigned sumarray(unsigned a[], int n) {
  int i; unsigned s;
  i=0;
  s=0;
  while (i<n) {
    s+=a[i];
    i++;
  }
  return s;
}

unsigned four[4] = {1,2,3,4};

int main(void) {
  unsigned int s;
  s = sumarray(four,4);
  return (int)s;
}
```

Before us is a simple program in the C programming language.
Approximately the second program in life that beginners write,
right after solving a quadratic equation.
The function finds the sum of array elements.
A very simple example, just what we need at the beginning.
But this function has a loop, which will be important for us later.

But why was C chosen specifically?
Mainly because of its simplicity.
"How so?" some will ask!
C is a very complex programming language!
That's why Python is more popular among beginners!
This is true, complexity comes in different forms.
C is simple as a shovel.
Python is simpler for digging a foundation pit for a house.
But no one would say that the construction of an excavator is simpler than a shovel.

Of course, the analogy is a simplification.
In the programming world, any shovel can be improved to infinity.
By the way, CPython is written in C.
But the point is more about the fact that from a mathematical perspective, construction simplicity is more important.
That's why algorithmic undecidability is proven in the BrainFuck programming language
(Turing machine), but try writing something on it!
Therefore, for formal verification, the simplicity of C is a blessing, not an obstacle.

How does verification work?
Usually we have two worlds.
The ideal platonic world of mathematics, where it's easy to build proofs.
And the harsh brutal world of programming, where your code crashes with terrible undefined behavior (UB).
And we, as always in fantasy works, have portals between worlds.
So, for example, the `unsigned int` type, which is limited by processor register capacity,
corresponds to the mathematical unlimited integer type ℤ,
which contains an infinite number of values.
Converting `unsigned int` to ℤ is quite easy — we take the corresponding number.
But backwards... there are options, but by default we just take the number modulo.
Then the sum of two integers will correspond to the sum modulo,
exactly as the processor performs it.
This portal works perfectly: we can first add numbers and then convert,
or first convert and then add, the result won't change.
Mathematicians love drawing commutative diagrams, this is the simplest example.

![Commutative Diagram](diag.png)

Here two paths from the upper left corner to the lower right are drawn.
The first path: first we add, then take modulo.
The second path: first we take modulo, then add.
And by the laws of the genre, if two paths lead to one point, the result is the same.

An array will correspond to a list.
The `int` type... will correspond to the same ℤ type.
But this time we won't be able to build such a beautiful commutative diagram.
Because `int` overflow according to the specification is UB.
Therefore, in the `sumarray` function, if we imagine that parameter `a` has type `int` instead of `unsigned`,
then proving that the function returns the sum of array elements will only be possible when
`n==0` or `n==1`.
If `n>1`, then... we'll need to add two numbers,
and this could be overflow, and everything breaks.

Where does verification start?
Since all our proofs will be in Coq, we need to get an AST
(Abstract Syntax Tree — the abstract syntax tree of our program).
This is done using the clightgen command:

```bash
clightgen -normalize sumarray.c
```

Yes, in my case it will complain

```
In file included from <built-in>:466:
<command line>:16:9: warning: redefining builtin macro [-Wbuiltin-macro-redefined]
   16 | #define __STDC_NO_THREADS__ 1
      |         ^
1 warning generated.
```

but still create the file `sumarray.v`.
This is a Coq file that contains the mathematical representation of our code.
Let's pause a bit on the descriptions of the static variable `four` and the `main` function:

```coq
Definition v_four := {|
  gvar_info := (tarray tuint 4);
  gvar_init := (Init_int32 (Int.repr 1) :: Init_int32 (Int.repr 2) ::
                Init_int32 (Int.repr 3) :: Init_int32 (Int.repr 4) :: nil);
  gvar_readonly := false;
  gvar_volatile := false
|}.

Definition f_main := {|
  fn_return := tint;
  fn_callconv := cc_default;
  fn_params := nil;
  fn_vars := nil;
  fn_temps := ((_s, tuint) :: (_t'1, tuint) :: nil);
  fn_body :=
(Ssequence
  (Ssequence
    (Ssequence
      (Scall (Some _t'1)
        (Evar _sumarray (Tfunction (Tcons (tptr tuint) (Tcons tint Tnil))
                          tuint cc_default))
        ((Evar _four (tarray tuint 4)) :: (Econst_int (Int.repr 4) tint) ::
         nil))
      (Sset _s (Etempvar _t'1 tuint)))
    (Sreturn (Some (Ecast (Etempvar _s tuint) tint))))
  (Sreturn (Some (Econst_int (Int.repr 0) tint))))
|}.
```

The prefixes `v_` and `f_` denote variables and functions respectively.
You can see a LISP-like AST with functions like `Ssequence`, `Scall`, `Sreturn`.
A bunch of attributes like `gvar_readonly`, `fn_callconv` are taken directly from the code and C standard.

In principle, this is just for general information about where Coq knows this from.
You can build this file with the command

```
coqc -Q . VC sumarray.v
```

Here `-Q . VC` means that the current directory will be available as the namespace `VC` in Coq.
As a result, many sumarray files with different extensions will be generated,
which will allow us to later import this file using the command `Require Import VC.sumarray.`

Next in our plans is to create a file `Verif_sumarray.v` which will contain specifications and correctness proofs.
There we will build the mathematical function `sum_Z`, which will find the sum of elements in a list of integers,
and prove that it's equivalent to our C function `sumarray`.

The first question immediately arises here.
So, suppose we proved that the functions are equivalent.
But what if we made mistakes in both implementations?
Yes, this is a good question, we can make mistakes in the formal specification.
But in the mathematical world we can prove more facts about `sum_Z`.
For example, that calling the function `sum_Z` on a list with one element will return that element.
Or that calling `sum_Z` on the concatenation of lists equals the sum of `sum_Z` calls for each list separately.

In the next code fragment there is a definition of the `sum_Z` function through the fold operation,
as well as proofs of these facts:

```coq
Definition sum_Z : list Z -> Z := fold_right Z.add 0.

Lemma sum_Z_one_elem : forall x, sum_Z [x] = x.
Proof.
  intro. simpl. lia.
Qed.

Lemma sum_Z_app:
  forall a b, sum_Z (a++b) =  sum_Z a + sum_Z b.
Proof.
  intros. induction a; simpl; lia.
Qed.
```

Now these facts are formally proven (you can ignore the Coq magic).
And now we don't have many options to make mistakes in such a way
that the `sum_Z` function satisfies these conditions while doing something different from summation.
But the fact remains, a mistake in the specification can nullify all our efforts.
It's only comforting that making a mistake in the specification is harder,
and also making it so that this mistake exactly corresponds to a mistake in the code
would be either sabotage or an unrealistic coincidence.

Now we're ready to give a formal specification of the `sumarray` function,
which will look like this:

```coq
Definition sumarray_spec : ident * funspec :=
DECLARE _sumarray
 WITH a: val, sh : share, contents : list Z, size: Z
 PRE [ tptr tuint, tint ]
  PROP  (readable_share sh; 0 <= size <= Int.max_signed;
         Forall (fun x => 0 <= x <= Int.max_unsigned) contents)
  PARAMS (a; Vint (Int.repr size))
  SEP   (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a)
 POST [ tuint ]
  PROP () RETURN (Vint (Int.repr (sum_Z contents)))
  SEP (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a).
```

Looks terrifying...
But, if you skip the Coq magic, then in the `WITH` section you can see entities from the ideal mathematical world,
which will correspond to `sumarray` variables:
`a` is the pointer to the array,
`contents` is the array itself, the memory content from address `a` with length `n * sizeof(unsigned)` bytes,
`size` is the variable `n`,
and `sh`... forget it, these are access rights to the memory that `a` points to.

The next section is `PRE`.
It contains conditions imposed on function arguments for the call to proceed correctly.
If we translate them into human language:
* `readable_share sh` — the memory that `a` points to must be readable,
* `0 <= size <= Int.max_signed` — the array size cannot be negative,
* `Forall (fun x => 0 <= x <= Int.max_unsigned) contents` — no restrictions on array `a` elements.

The next section is `POST`.
It contains statements that will automatically be fulfilled after the function completes.
Of course, provided that all `PRE` conditions were met at the time of the call.
If we translate them into human language:
* `RETURN (Vint (Int.repr (sum_Z contents)))` — the function will return the value `sum_Z contents`, the sum of all array elements,
* `SEP (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a)` — the array will remain unchanged after function execution.

In principle, the conditions look quite logical, we just need to prove this.
Here's how it's done:

```coq
Lemma body_sumarray: semax_body Vprog Gprog f_sumarray sumarray_spec.
Proof.
  start_function.
  forward. (* i = 0; *)
  forward. (* s = 0; *)
  forward_while
   (EX i: Z,
     PROP  (0 <= i <= size)
     LOCAL (temp _a a;
            temp _i (Vint (Int.repr i));
            temp _n (Vint (Int.repr size));
            temp _s (Vint (Int.repr (sum_Z (sublist 0 i contents)))))
     SEP   (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a)).

  - (* Check invariant before *)
    Exists 0. entailer!.

  - (* i<n *)
    entailer!.

  - (* loop body *)
    assert_PROP (Zlength contents = size). {
      entailer!.
      do 2 rewrite Zlength_map. reflexivity.
    }
    forward. (* t' = a[i] *)
    forward. (* s = s + t' *)
    forward. (* i = i + 1 *)
    Exists (i+1).
    entailer!. f_equal. f_equal.
    rewrite (sublist_split 0 i (i+1)) by lia.
    rewrite sum_Z_app. rewrite (sublist_one i) by lia.
    simpl. lia.

  - (* Continue execution after loop *)
    forward.  (* return s; *)
    entailer!.
    autorewrite with sublist in *|-.
    autorewrite with sublist.
    reflexivity.
Qed.
```

Let's get familiar with the proof structure.
This is very similar to debugging.
Only when we execute the `next` command in `gdb` (or choose Debug -> Step in the IDE menu),
we execute a command with specific variable values.
And when we execute the `forward` tactic, we move to the next command
for all possible variable values.
When we execute `forward` on the statement `i=0`, the fact that `i==0` will be added to our fact base.
Same with `s`.
It's also not hard to execute an `if` with `forward` (you'll need to go through branches sequentially).
But difficulties begin with loops: we can't go through them sequentially,
because different function calls may have different numbers of iterations.

But there's a way out!
It's called the "loop invariant".
This is a statement that remains true after each iteration.
The bad news is that no one will help with what loop invariant will be useful.
In this case, the invariant is quite obvious and consists of two statements:
(1) `0 <= i <= size` — the counter is within the array bounds,
(2) the sum of numbers from 0 to `i` in array `a` equals `s`.
Or in Coq language: `PROP (0 <= i <= size)` and `temp _s (Vint (Int.repr (sum_Z (sublist 0 i contents))))`.
There `sum_Z (sublist 0 i contents)` is our functional call to the sum_Z function, preceded by some magic.

Now we need to prove that (1) the invariant holds at the beginning of the loop;
(2) the loop condition executes without UB;
(3) the loop invariant is preserved when executing the loop with the additional condition `i<n` that we didn't exit the loop — here we need to go through the loop once in our "debugger".
And that's it, we can add the invariant and loop exit condition to the fact list and continue the proof.
In Coq terminology, this means we need to prove four goals: (1)-(3) and goal (4) continuation after the loop.
The beginning of proof for each new goal starts with the minus symbol `-`.

First goal: the invariant holds at the beginning.
The proof is trivial because we already know that `i==0` and `s==0`, and the sum of zero elements is zero.
Just some magic that synchronizes the worlds.

Second goal: the loop condition executes without UB.
Also trivial — comparing two `int`s is defined for any values.
`entailer!.` here means simplify everything possible, after which the result will be obvious.

Third goal: the invariant is preserved.
Here we execute the loop body step by step.
Note that the two statements `s+=a[i]; i++;` were replaced by three actions: `t'=a[i]; s=s+t'; i=i+1;`
And then again some magic — we prove that the sum of elements from 0 to i+1
equals the sum of elements from 0 to i plus the element at index i.
For this we use the lemma `sum_Z_app` that we proved at the beginning.

Now the last goal, we execute `return s;` through `forward`
after which from three facts: `i >= n` (loop exit condition),
`0 <= i <= size` (from the invariant) and `size = n` (from PRE conditions)
we conclude that `i = n`, hence s equals the sum of all array elements.

Congratulations, we verified our first function!

Let's do a small review.
The proof was, in principle, quite straightforward.
The only problem was finding the invariant.
And even for this example it was quite obvious,
but in other more practical cases this can be a real challenge.

Let's take as a more non-obvious example the well-known list reversal problem:

```c
struct node* reverse(struct node* list) {
    struct node* result = NULL;
    struct node* remaining = list;

    while (remaining != NULL) {
        struct node* current = remaining;
        remaining = remaining->next;
        current->next = result;
        result = current;
    }

    return result;
}
```

Here the invariant would be `reverse(result) ++ remaining == list`.
Upon loop exit `remaining` will be empty, so we get `reverse(result) == list`.
We can see that the formula is quite non-obvious, and cannot be obtained automatically, finding it is the creative work of the developer.

It's interesting to draw parallels here with another verification method implemented in SPARK (Ada).
Let's look at an example

```ada
with Loop_Types; use Loop_Types;

procedure Zero_Arr (A : out Arr_T) with
    SPARK_Mode,
    Post => (for all J in A'Range => A(J) = 0)
is
begin
   for J in A'Range loop
      A(J) := 0;
      pragma Loop_Invariant (for all K in A'First .. J => A(K) = 0);
   end loop;
end Zero_Arr;
```

Here we see verified array zeroing.
Looks a bit more humane: we see pre and post conditions and loop invariant in the code,
not in a separate file.
The principle is similar: we write a loop invariant,
and then SPARK itself tries various steps to prove the postcondition.
The only difference is that if invariants don't help, nothing can be done.
And in case of Coq we can (actually must) prove any lemma manually, if it can be mathematically proven at all.
Well, if you have enough skill and patience.

In our example we wrote the function twice:
once in the ideal world in Coq,
and once in the brutal world of C programming,
and then proved equivalence.
Is it always necessary to do the work twice?
No, it's not mandatory.
For example, for sorting verification it's enough to prove two properties:
for any element its count will not change,
and each element is not greater than the next one.
Such properties leave no other options:
the function either sorts or fails verification.

So what does Coq give us?
First, the semantics of the C programming language is formally described there.
Since C is simple as a shovel, this is a task that, although complex, is manageable as we can see.
I don't want to think about other languages here.
Second, various tactics are implemented there that allow us to walk through the AST tree
and perform some boilerplate, although we'd like more.

Perhaps to some this might seem like child's play.
Indeed, we proved elementary stuff!
But you have to start with elementary things.
If anyone is interested in more serious things, I suggest looking at the
[catalog of all VST examples](https://github.com/PrincetonUniversity/VST/blob/master/doc/catalog-of-examples.md).
There's verification of a generational garbage collector for programs with graphs,
concurrent messaging system and much more.

Well, that's all, I think I've entertained you enough.
Now you should have a slightly better idea about verification.
For those interested in going further, here are the links:

[Software Foundations](https://softwarefoundations.cis.upenn.edu/) — a fundamental course on formal methods and program verification.
Consists of several volumes, from logic basics to practical applications.
Volume 5: Verifiable C is dedicated specifically to VST and contains much more examples and details than our article.
There you can also find how to prove list operations, track malloc/free — things that Ada+SPARK does poorly.

[Official VST manual](https://vst.cs.princeton.edu/download/VC.pdf) with detailed explanations and examples.

[I can't believe that I can prove that it can sort](https://blog.adacore.com/i-cant-believe-that-i-can-prove-that-it-can-sort) — AdaCore article about verifying sorting algorithms in SPARK.

Examples can be found on [github](https://github.com/mustitz/blabber/blob/master/vst/).
