# VeRA Artifact Evaluation

Hello, and thanks for evaluating this artifact!

Our paper evaluation (Section 6) has six parts:

1. Can VeRA prove Firefox range analysis correctness?
2. Can VeRA proofs catch real correctness bugs?
3. Are the VeRA proofs correct?
4. Do the verified routines work correctly in Firefox?
5. How do the verified routines perform in Firefox?
6. How hard is it to integrate the verified routines into Firefox?

We are able to reproduce the results from points 1-5 (the sixth is a bit more subjective).
Note that all timing results may vary slightly, as will the counterexamples the
solver chooses to display for buggy routines.

## Install
This is for reference setting up the vm, and should be removed probably.
Need pdflatex and python2 !!

## Clean the results directory

From the **results** directory, run `python clean.py`. This will get rid of all results
and all intermediate files (there shouldn't be any to begin with, but you can use this
anytime to clean up the directory).

## Getting started

Make sure you are in the **results** directory!

Run: From the **results** directory, `python sanity.py`

Expected result: Two files show up, verif_sanity.txt and ff_sanity.txt.
verif_sanity.txt should have a bunch of output followed by **1 out of 16 tests failed**.
ff_sanity.txt should have....

Expected time: under a minute

This does a single run of a number of tests and verification routines. If the output
differs from the expected output, please indicate so on HotCRP so we can quickly fix it. 

## Generate results for the entire paper at once

Make sure you are in the **results** directory!

From the **results** directory, type `python repro_all.py` to reproduce all
results in the paper.

Expected time: XXXX

The results will all be generated in the results directory, in the following order:
1. Can VeRA prove Firefox range analysis correctness?
   Compare **results/verify_table.pdf** to Figure 8 in the paper.
2. Can VeRA proofs catch real correctness bugs?
   Compare **results/bug_examples.txt** to 6.1's **An old Firefox bug** and
   **A new Firefox bug** bug examples. 
3. Are the VeRA proofs correct?
   Compare **results/quickcheck.txt** to 6.1's **Are VeRA proofs correct?**.
   Note that by default, our script runs quickcheck only 100 times per operator,
   while for the paper we ran 1,000 times per operator. We provide an optional
   way of running 1,000 quickcheck tests per operator below.
4. Do the verified routines work correctly in Firefox?
   Compare **results/firefox-js-tests.txt** to 6.2's **Do the verified routines
   work correctly?**.
5. How do the verified routines perform in Firefox?
   Compare **results/jetstream2.pdf** to 6.2's **How do the verified routines perform?**.

Alternatively, to generate results for each claim individually, use the
following instructions:

### (1) Can VeRA prove Firefox range analysis correctness?

Run: make_verif_table.py

Look at: results/verify_table.pdf

Compare to: Figure 8

Expected time: Overnight

The script for generating Figure 8 is called make_verif_table.py This
script uses command `stack test --ta '-p Verification',` which verifies
each range analysis operator correct wrt to JavaScript semantics with
a timeout of 20 minutes. It will generate a standalone PDF of the time
it took each verification condition to verify in verify_table.pdf

### (2) Can VeRA proofs catch real correctness bugs?

Run: generate_bugs.py

Look at: results/bug_examples.txt

Compare to: 6.1's **An old Firefox bug** and **A new Firefox bug** bug examples

Expected time: 2-3 minutes 

The script for generating the examples in 6.1's "A new Firefox bug" and
"An old Firefox bug" are in generate_bugs.py This script uses command
`stack test --ta '-p <test>'`, where `test` is brokenIntersectTest or
brokenCeilTest. This runs verification rountines for
either buggy operator and displays (1) a counterexample showing that each
operator is buggy and (2) the time it took to generate that example. The
output will be in bug_examples.txt

### (3) Are the VeRA proofs correct?

Run: quickcheck.py

Look at: quickcheck.txt

Compare to: 6.1's **Are VeRA proofs correct?**

Expected time: ~30 minutes 

The script for generating the quickcheck tests in 6.1 is in quickcheck.py.
It runs the command `stack test --ta -p JS_Fast/Cpp_Fast`. 
By default, the script runs **100** random tests for each JS or C++ operator.
In the paper, we run quickcheck tests 1,000 times per operator---we do not
do so for time reasons in the artifact eval. If you would like to run quickcheck
for longer, you can use quickcheck_long.py in the same way as quickcheck.py. 
Still, it will not be the exact result from the paper, since each run of quickcheck
produces new random tests.

### (4) Do the verified routines work correctly in Firefox?

Run: firefox-js-tests.py

Look at: firefox-js-tests.txt

Compare to: 6.2's **Do the verified routines work correctly?**

Expected time: ~30 minutes

Firefox has 3 main test suites for Javascript and the JIT: `jstests`,
`jsapi-tests`, and `jit-test`. The script runs all three and logs their
results.

### (5) How do the verified routines perform in Firefox?

Run: jetstream2.py

Look at: `jetstream2.pdf`

Compare to: 6.2's **How do the verified routines perform?**

Expected time: ~30 minutes 

This script executes the JetStream2 benchmark suite for both versions of firefox
and produces a graph comparing the results. **TODO about latency test**

