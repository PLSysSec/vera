# VeRA Artifact Evaluation

Hello, and thanks for evaluating this artifact!

The evaluation has six parts:

1. Can VeRA prove Firefox range analysis correctness?
2. Can VeRA proofs catch real correctness bugs?
3. Are the VeRA proofs correct?
4. Do the verified routines work correctly in Firefox?
5. How do the verified routines perform in Firefox?
6. How hard is it to integrate the verified routines into Firefox?

We are able to reproduce the results from points 1-5.
Note that all timing results may vary slightly, as will the counterexamples the
solver chooses to display for buggy routines.

## Install
This is for reference setting up the vm, and should be removed probably.
Need pdflatex and python2 !!

## Generate results for the entire paper at once

From the **results** directory, type `python repro_all.py` to reproduce all
results in the paper. Expecte this to take XXXX.

The results will all be generated in the results directory, in the following order:
1. Can VeRA prove Firefox range analysis correctness?
   Compare **results/verify_table.pdf** to Figure 8 in the paper.
2. Can VeRA proofs catch real correctness bugs?
   Compare **results/bug_examples.txt** to 6.1's **An old Firefox bug** and
   **A new Firefox bug** bug examples. 
3. Are the VeRA proofs correct?
   Compare **results/quickcheck.txt** to 6.1's **Are VeRA proofs correct?**.
   Note that by default, our script runs quickcheck only 100 times per operator,
   while for the paper we ran 1,000 times per operator. This is configureable (see
   section 3 below), but we don't reccomend increasing it for time reasons. 
4. Do the verified routines work correctly in Firefox?
   TBD   
5. How do the verified routines perform in Firefox?
   TBD

Alternatively, to generate results for each claim individually, use the
following instructions

### (1) Can VeRA prove Firefox range analysis correctness?

Run: make_verif_table.py

Look at: results/verify_table.pdf

The script for generating Figure 8 is called make_verif_table.py This
script uses command `stack test --ta '-p Verification',` which verifies
each range analysis operator correct wrt to JavaScript semantics. It
will generate a standalone PDF of the time it took each verification
condition to verify in verify_table.pdf

### (2) Can VeRA proofs catch real correctness bugs?

Run: generate_bugs.py

Look at: results/bug_examples.txt

The script for generating the examples in 6.1's "A new Firefox bug" and
"An old Firefox bug" are in generate_bugs.py This script uses command
`stack test --ta '-p <test>'`, where <test> is brokenIntersectTest of
brokenCeilTest. This runs verification rountines for
either buggy operator () and displays (1) a counterexample showing that each
operator is buggy and (2) the time it took to generate that example. The
output will be in bug_examples.txt

### (3) Are the VeRA proofs correct?

Run: quickcheck.py

Look at: quickcheck.txt

The script for generating the quickcheck tests in 6.1 is in quickcheck.py.
It runs the command `stack test --ta -p JS/Cpp`. 
By default, the script runs **100** random tests for each JS or C++ operator.
In the paper, we run quickcheck tests 1,000 times per operator---we do not
do so for time reasons in the artifact eval. If you would like to run quickcheck
for longer, you can configure it by WHATTTTT:DS. 

### (4) Do the verified routines work correctly in Firefox?

### (5) How do the verified routines perform in Firefox?

## Other source code information for the interested

