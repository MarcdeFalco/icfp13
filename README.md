TaupeGoons code for ICFP 2013
=============================

Postmortem
----------

### Some words about the team
This year, the TaupeGoons was a first-year teacher in maths and cs (Marc de
Falco http://marc.de-falco.fr) and seven of its students (Kévin Coussemacker,
Kévin Goilard, Jean-Yves Franceschi, Marc Debiard, Grégoire Bonin, Blaise
Raybaud, Lucas Pascal).

The task was soon overwhelming for the students, so all code was written by 
Marc while taking suggestions from the students. We started the ICFP nicely
with a BBQ and a swimming pool party. Then, shit got real.

### Horsepower
We had no access to great computers so we ended doing all computations on
Marc's i5 (quad core 3.2ghz) with 16gb of ram.

* **1166** lines of OCaml
* **63** lines of Python
* **22** hours of coding
* **50** core-hours because I was late to embrace multicores
* **1202** points on final score
* **150** the number of points burned stupidly because the final solver would
  have scored them

### Statistics of the generator
All time is measured on one core of an i5 @ 3.2ghz using 16gb of ram.

Those stats only take into account the generation and not the validation of the
programs.
#### Programs using and shl1
only one binary and one unary is easy.

* size 14: 112081 programs generated in 80ms using 5mb
* size 18: 7986474 programs generated in 7s using 6mb
* size 20: 71374814 programs generated in 1m using 6mb
#### Programs using and or xor
starting from now I don't give the memory usage, it's always 6mb or less.

* size 14: 2466264 programs generated in 1.8s
* size 16: 39119265 programs generated in 29s
#### Programs using not shl1 shr1 shr4
* size 12: 1101180 in 1.5s
* size 13: 4174887 in 4.8s
#### Programs using if0 and shl1
* size 13: 2518544 in 0.42s
* size 16: 240945855 in 27s
### Our method
#### First : brute-force with memory
The first method we did implement was a classic brute-force :

1. generate all programs below a given size
2. filter out bad programs given randomly generated examples
3. try the first program and if there is a mismatch add it to the examples

Obviously it did not scale well because for large size I needed to store every
programs before trying to make a guess. 

The generation is recursive top-down with programs constructed bottom-up while resolving recursion.
#### Our (only) idea : continuation passing style
##### Generating trees without memory
First, let's look at the previous generation for a basic tree type in
pseudo-caml:

    let rec gen size =
        if size = 0
        then Leaf
        else 
             fold concat [
                [ Node (tree_left, tree_right) for tree_right in gen
                        (size-1-size_of tree_left) ]
                for tree_left in gen (size-2)
             ]

The idea is to start generating the left tree and when a left tree is found, to
call a continuation for building the right tree:

    let rec gen size cont =
        if size = 0
        then cont Leaf
        else gen (size-2)
            (fun tree_left ->
                gen (size-1-size_of tree_left)
                    (fun tree_right ->
                        cont (Node (tree_left, tree_right))))

Then the first continuation given to the generating function is the one testing
examples. This way, as soon as a tree is built it is tested.

We could generate trees up to commutativity by replacing the call *gen
(size-2)* by *gen ((size-1)/2)*.

##### Going into full-fledged program
We adapt the previous method with some twists :

* to avoid generating stupid program like 0+x we have two equivalence testing :
  Equiv.zero and Equiv.one. Also we avoid stacking not operators because
  not(not x) ~ x.
* to be ablet o run in parallel many generations without searching in the same
  order, the branching between searching for a leaf, a unary, a binary, ... is 
  randomized.
#### Some note about bonus problems
For bonus problems who are always starting by a if0, we use the following
algorithms:

1. Generate a lot of example (512)
2. Iterate over all programs until we find one matching at least one half of
   the examples. We partition the examples between the good set and the bad set
   not matching this program. The generated program is referred to as
   good_branch.
3. We try to search an expression of value 0 on all the good set and not 0 on
   the bad set (or the reverse).
4. When a condition is found, we go for a third expression for the bad_branch
   matching the bad set.
5. When all is found we try if0(condition,good_branch,bad_branch) if there is a
   mismatch we add the new example and we break out of 3 and 4.

Compiling and running
---------------------
to compile you need OCaml with some libraries and utilities the easiest thing is to get opam and those modules :

* oasis, a builder for OCaml
* yojson, a json parser/writer
* libcurl, the infamous
* uint, to get uint64 type

then you do 

* ./configure
* ./compile

to run it you can use a python script launcher.py who launches 4 processes and monitors them :

* python launcher.py <HASH>

for a bonus problem:

* python launcher.py <HASH> b

