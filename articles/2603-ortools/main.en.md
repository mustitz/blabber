# Google OR-Tools: Bughouse Tournament Pairing

I recently had a practical problem that I solved with the help of Gemini Code agent and the OR-Tools library.
I decided to describe this experience in an article.
It might be useful to see what class of problems OR-Tools solves.
And when you encounter something similar, you'll be able to recognize the pattern and use this tool instead of reinventing the wheel.
I'll also share my impressions of working with an LLM, maybe someone will find that interesting too.

## Bughouse

There's this interesting game called bughouse chess.
As you might guess, it's a variant of chess, but not one-on-one — it's two-on-two.
The twist is that when your partner captures a piece on their board, they pass it to you.
On your turn, you can place this piece almost anywhere on your board.
Checkmate on either board means the team wins.

It's a very interesting, fun, fast, and dynamic game.
You have to constantly communicate with your partner: "give me a knight", "anything but a queen", because often placing the right piece decides the game's fate.
Both beginners and professionals love playing this game.
You can see what it looks like in action (fourth game of the final match, Berlin, 2009):

[![Bughouse chess game](bughouse-preview.jpg)](https://www.youtube.com/watch?v=mMxERRP7R3g)

Sometimes we gather at our chess club to play bughouse.
But... there are certain problems.
Making fixed pairs... we have to, because what else can you do?
But this has many drawbacks: playing all day paired with a beginner is quite the quest.
Strong players want to play with strong players against strong players, but there aren't that many players.
Another problem: if the number of people isn't divisible by four, someone won't play.

Obviously, any chess player would immediately start dreaming of something analogous to the Swiss system,
where in the next round players are paired according to results:
leaders against leaders, mid-tier against mid-tier.
However, I couldn't find any ready-made programs like that.
But I'm a programmer!
Plus there's hope in LLMs.

## Problem Formalization

Let's think a bit about approaches to solving this problem.
So, we have a list of participants.
We have previous results (and ratings).
We need to create a schedule for the current round: who plays with whom and against whom.

Essentially, among all possible pairings, we need to choose the best one.
How can we solve this problem?
First, we can try to create an algorithm that builds the pairing we need,
adding at each step the pair that is optimal at that moment.
This is essentially how people conducted Swiss system pairings before computers existed.
Yes, we can hit a dead end where all options are bad.
Then we'll have to backtrack and break up the previous pairing.
This is called greedy search with backtracking, essentially we're talking about a custom implementation
tailored to a specific problem.

The second solution is more mathematical: for every flaw we see in the pairing, we'll assign a penalty.
And then we have a mathematical optimization problem: among all schedules, find the one with minimum penalty.

Both approaches are similar in some ways, both make sense.
Gemini Code initially chose the first option.
This is exactly how, for example, the FIDE-approved Dutch system for Swiss pairings works.
But this works well when the penalty system is stable.
If we're talking R&D, which is what we have, it's quite unclear which penalties will give good results.
Any experiments with penalties mean reworking the search algorithm, which can be expensive.
So all attempts by the LLM to go down that path were decisively blocked. I asked a very narrow question:
what kind of mathematical problem is this, what solution methods exist, what Python libraries are there.
And among the described options, I found what interested me: OR-Tools.

What is OR-Tools?
It's a package for solving pseudo-Boolean optimization problems.
We have Boolean variables (regular bits).
We have constraints on them, meaning some bit combinations are allowed, some are forbidden.
And we have a function that evaluates each allowed option, called the objective function.
The solver finds exactly the bit combination that is allowed and gives the best value of the objective function.

## Implementation

Thus we just need to find a way to encode the pairing as a sequence of bits.
After that everything becomes simpler: we just play with penalties and evaluate pairing results.
No algorithm changes!

So, how exactly do we encode the pairing with bits?
This is the question I asked the LLM after understanding the basics of OR-Tools.
It handled the task, but not optimally.
Anyway, first, we need to define pairs of players who play on the same team.
Let's say we have 8 players.
One pair can be created in C(8, 2) = 28 different ways.
So we'll have 28 bits, each corresponding to each pair.
Graphically this can be represented as a table or matrix, where we're only interested in elements above the diagonal.

![Matrix of possible pairs](pairs-matrix.png)

Obviously, we can't set all 28 bits to one.
Each player should have only one partner.
This means that if we take all possible pairs for one player, only one bit can be set there.
In the picture, gray shows unavailable positions (diagonal and below), green shows possible player pairs, red shows all possible pairs for player 3.
Exactly one of the red bits must be set to one.

With opponents it's the same, except exactly two bits need to be set for each player.
We also need to add a condition that a partner can't be an opponent.
This is achieved with the condition: "if two players are partners, then the bit for their opposition must be cleared".
Finally, the last constraint: if two players are partners, their opponents must match.
Otherwise we might get a situation where player A plays with partner B against players C and D, but player B is somehow playing against E and F.
That's all, now let's just see how this looks in OR-Tools language:

```python
model = cp_model.CpModel()
is_team, is_opp = {}, {}

for i, j in pairs(range(qactive), 2):
    teamvar = model.NewBoolVar(f't_{i}_{j}')
    oppvar = model.NewBoolVar(f'o_{i}_{j}')
    is_team[i, j] = teamvar
    is_team[j, i] = teamvar
    is_opp[i, j] = oppvar
    is_opp[j, i] = oppvar
    model.Add(teamvar + oppvar <= 1)
```

Let's break down this code a bit.
`cp_model.CpModel()` creates a model or problem that we'll be solving.
`is_team` and `is_opp` are dictionaries, these are Boolean variables that indicate who are partners and who are opponents.
The key is a pair of indices `(i, j)`, where `i` and `j` are player indices from 0 to `qactive-1` (`qactive` is the number of players).
To not worry about index order, we immediately add two elements to the dictionary.

We also add a condition that a partner can't be an opponent.
This is done with the line `model.Add(teamvar + oppvar <= 1)`
Often in mathematics, bits are not `True` or `False` values, bits are integer values `0` or `1`.
So they can be treated as integers.
That's exactly what we do here when we write the condition `teamvar + oppvar <= 1`.
Obviously, this condition can only be violated when both `teamvar` and `oppvar` are set to one.
That is, a partner can't be an opponent.

It should also be noted that often when you see `teamvar + oppvar` you subconsciously expect a computation to be performed here.
In this case, it's not.
`teamvar` and `oppvar` have the type of OR-Tools model variable, for which the `__add__` operation is overloaded,
so `teamvar + oppvar` forms an expression, for which the `__le__` operation is also overloaded.
Thus no actions will be performed, a tree will be built which will be passed to the model.Add method.
This is similar to how libraries like NumPy or SymPy work:
instead of immediately computing the result, they build a computation graph,
which can then be optimized and executed.

```python
for i in range(qactive):
    model.AddExactlyOne([is_team[i, j] for j in range(qactive) if i != j])
    model.Add(sum(is_opp[i, j] for j in range(qactive) if i != j) == 2)
```

Next are fairly obvious conditions that there should be only one partner and two opponents.
To illustrate different methods of adding constraints, both `AddExactlyOne` is used
(exactly one of the listed bits must be set),
and the familiar `Add` which uses the built-in `sum` that will call the overloaded `__add__` method for variables,
and the overloaded `__eq__` method for the expression and constant.

```python
for i, j in pairs(range(qactive), 2):
    for k in range(qactive):
        if k == i or k == j:
            continue
        model.Add(is_opp[i, k] == is_opp[j, k]).OnlyEnforceIf(is_team[i, j])
```

And finally the condition that links all participants at one table.
It's better to read this from the end: if players `i` and `j` are partners, then player `k` must be an opponent for both, or for neither.
I think this is illustrative enough to understand how `OnlyEnforceIf` works:
the condition only applies when the variable is set.

**Exercise for the reader:** create 64 Boolean variables for an 8×8 chessboard and write conditions for placing eight queens so they don't attack each other.
Think about how to implement constraints: exactly one queen on each row, exactly one queen on each column, at most one queen on each diagonal.

Gemini basically solved the problem, but... not optimally.
Besides partnership and opposition variables, it created an additional entity — the table.
We additionally got variables "player _i_ sits at table _k_".
And this is a very big hit to performance.
Even if we don't consider the number of variables, we got quite a lot of symmetries,
because no matter how you seat players at tables, it doesn't really change anything.
But for the algorithm this can be terrible, because many options will have almost the same penalty values.

But, to the comment "Are you crazy?" Gemini quickly fixed the mistake.

Moving on, the next step is evaluating arrangements.
This is done by the following code:

```python
obj_terms = []
for i, j in pairs(range(qactive), 2):
    p1, p2 = squad[i], squad[j]
    t_cost = sum(w * f(p1, p2, stats, groups) for w, f in team_fines)
    o_cost = sum(w * f(p1, p2, stats, groups) for w, f in opp_fines)
    obj_terms.append(is_team[i, j] * int(t_cost))
    obj_terms.append(is_opp[i, j] * int(o_cost))

model.Minimize(sum(obj_terms))
```

As we can see, this code is quite simple:
for each pair of players we assign a penalty for how undesirable it is to pair them as partners and as opponents.
`t_cost` shows how undesirable it is for players _i_ and _j_ to play on the same team.
Correspondingly `o_cost` shows how undesirable it is for players _i_ and _j_ to be opponents.
This depends on history (how they've crossed paths before), on results (remember that leaders should play against leaders).
All this is encoded in the state, which is described in variables `squad` (list of active players), `stats` (their statistics), `groups` (groups by points).

`is_team[i, j] * int(t_cost)` — again we see an overloaded operator: if the bit is set, remember that mathematically it's one, then we add the penalty.
If it's zero, then zero multiplied by the penalty zeros out the penalty.

Penalties are the most applied part, they require domain understanding.
They are what drive the pairing process.
I found that this system gives pretty good results:

```python
def deny_same_partner(p1, p2, stats, groups):
    return 1 if p2 in stats[p1].partners else 0

def deny_same_opp(p1, p2, stats, groups):
    return stats[p1].opps.count(p2)

def pts_mismatch(p1, p2, stats, groups):
    return abs(stats[p1].points - stats[p2].points)

def team_same_points_pattern(p1, p2, stats, groups):
    s1, s2 = stats[p1], stats[p2]
    if s1.points != s2.points:
        return 0
    group = groups[s1.points]
    n, d = len(group), abs(group.index(p1) - group.index(p2))
    return abs(d - n / 2.0)

def opp_same_points_pattern(p1, p2, stats, groups):
    s1, s2 = stats[p1], stats[p2]
    if s1.points != s2.points:
        return 0
    group = groups[s1.points]
    n, d = len(group), abs(group.index(p1) - group.index(p2))
    return min(abs(d - n / 4.0), abs(d - 3.0 * n / 4.0))

team_fines = [
    (1000000, deny_same_partner),
    (   1000, pts_mismatch),
    (      1, team_same_points_pattern),
]

opp_fines = [
    (1000, deny_same_opp),
    (1000, pts_mismatch),
    (   1, opp_same_points_pattern),
]
```

`deny_same_partner` is the prohibition of players playing on the same team if they've already played together.
Prohibition because the weight is 1000000.
Yes, we could have prohibited it at the structure level, but... if 8 players decide to play a 10-round tournament, then... why not?
I don't know what it would look like, but after seven rounds there will be repeats.

Next come three penalties with the same weight coefficient 1000.

The first is `deny_same_opp`, we want to avoid pairing the same opponents.
As you can see, we're quite tolerant of opponent repeats.
First, there are twice as many opponents as partners.
Second, for 8 players, in the fourth round we'd already have forced pairing of who's left.
But that's a matter of taste.

The second penalty is `pts_mismatch`, works for both partners and opponents.
We want approximately the same number of points on the team and in opposition.

The last penalty is `opp_same_points_pattern` and `team_same_points_pattern`.
This is the seating pattern for players in the same points group.

Why? If we seat randomly, leaders might start playing against leaders from the first round.
This creates a lot of randomness.
If we make even pairs (1 vs 2, 3 vs 4), then after the first round the placement will be random.

But we want strong players to rise to the top.
So a certain pattern is used.
For example, for 16 players with known ratings in the first round, the algorithm will output:
1+9 vs 5+13, 2+10 vs 6+14, 3+11 vs 7+15, 4+12 vs 8+16.
That is, stronger players will have more chances to win, though anything can happen.

Interestingly, Gemini was really struggling in the applied part.
Chess threw her off, she tried to force analogies from there.
She wanted to give the seating pattern a weight of 1000 for company, which is complete idiocy.
Moreover, if a person understands they don't know the subject, they behave modestly, ask questions, but Gemini...
Was just an example of militant ignorance.
Yes, it's understandable, bughouse isn't what the internet is stuffed with.
But an interesting point worth keeping in mind: when you go beyond mainstream topics, LLMs can give very confident but absolutely wrong advice.

The last step is running the search.
For this we just need to create a Solver and run it:

```python
solver = cp_model.CpSolver()
solver.parameters.max_time_in_seconds = 20.0
status = solver.Solve(model)
```

Here we set the maximum execution time to 20 seconds.
If the solver doesn't find an optimal solution in that time, it will return the best found option.
For complex problems this is important, because otherwise the search can take a very long time.

Now we get and print the result:

```python
if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
    for i, j in pairs(range(qactive), 2):
        team_value = solver.Value(is_team[i, j])
        opp_value = solver.Value(is_opp[i, j])
        print('Pair', i, j, '->', team_value, opp_value)
```

The solver can return different statuses:
`OPTIMAL` means an optimal solution was found, the best possible.
`FEASIBLE` means a feasible solution was found, but not necessarily the best, this usually happens when time runs out.
In both cases we have a working solution and can use it.

**Exercise:** finish the 8 queens example and print one arrangement.

**Exercise:** using a callback class (inheriting from `cp_model.CpSolverSolutionCallback`) and the parameter `solver.parameters.enumerate_all_solutions = True`, get all queen arrangements.

## Resources

Of course, running a tournament from the command line will be inconvenient for most people.
So we need an interface.
Too bad I'm not an expert at this.
But, oh miracle! As soon as I described what I wanted, Gemini instantly produced the needed working code, exactly as I asked.
Works well, I don't understand this stuff.

Full code is available on GitHub: [mustitz/bughouse-swiss](https://github.com/mustitz/bughouse-swiss)

There, besides pairing, there's a lot of auxiliary code, debugging, simulations, etc.

You can check out the ready site here: [bughouse-swiss demo](http://mustitz.host.funtoo.org:5001/)

![Web interface for pairing](bughouse-cite.png)

## Summary

Time to summarize.

The main benefit, in my opinion, is that you'll know about the existence of OR-Tools.
And if a similar problem arises (combinatorial optimization with constraints), you'll understand where to dig.
We saw how to formulate a problem through Boolean variables and constraints.
How to assign penalties for soft conditions.

I also entertained you a bit with tales about LLMs.
