
# VeRA code 

## Install dependencies

- z3
- The Haskell tool [Stack](https://docs.haskellstack.org/en/stable/README/)

## Build project

One you have all the dependencies installed, you can build the project:
```
stack build
```

## Run verification

To try to verify all the range analysis routines, run:
```
stack test --ta '-p Verification'
```

This will take a long time (overnight-ish)!

## Code map

Source:
-- ActiveCode: Generate QuickCheck tests for C++/JS semantics
-- DSL: Implementation of C++ and JavaScript semantics
-- Generate: Generate SMT from C++
-- IonMonkeyGenerated:
---- code.cpp: Firefox's range analysis routines in VeRA C++
---- Operations: Connecting the VeRA C++ implementations to Haskell so we can verify them
---- Verify: Verification code

# VeRA Artifact Evaluation (instructions from the AE submission)

Hello, and thanks for evaluating this artifact!

Our paper evaluation (Section 6) has six parts:

1. Can VeRA prove Firefox range analysis correctness?
2. Can VeRA proofs catch real correctness bugs?
3. Are the VeRA proofs correct?
4. Do the verified routines work correctly in Firefox?
5. How do the verified routines perform in Firefox?
6. How hard is it to integrate the verified routines into Firefox?

We are able to reproduce the results from points 1-5 (the sixth is a bit more subjective).
Note that all timing results may vary, as will the counterexamples the solver chooses to
display for buggy routines.

**WARNING: Expect to need around 110 GB of free disk space to run this evaluation.**
We're sorry, it contains multiple versions of the Firefox browser.

## Setup

1. Install virtualbox from virtualbox.org

2. Use the 'import appliance' option from the file menu in virtualbox to import
   vera.ova. You can either accept or modify the default options. 

3. Push the start button at the top of the screen to start the vm.

4a. For a graphical interface, a login screen will pop up after starting the vm.
    Login with username: vera and password: vera_user

4b. For a non-graphical interface, you can connect to port 22222 on localhost: 
    ssh -p 22222 vera@127.0.0.1 with password vera_user 

5. Navigate to the ~/lejit/results directory 

## Clean the results directory

From the **results** directory, run `python2 clean.py`. This will get rid of all results
and all intermediate files (there shouldn't be any to begin with, but you can use this
anytime to clean up the directory).

## Getting started

Make sure you are in the **results** directory! All python scripts should be run with
python2. 

Run: From the **results** directory, `python2 sanity.py`

Expected time: under a minute

Expected result: verify_sanity.txt has a bunch of output followed by
**1 out of 16 tests failed**.

This does a single run of a number of tests and verification routines. If the output
differs from the expected output, please indicate so on HotCRP so we can quickly fix it. 

## Generate results for the entire paper at once

Make sure you are in the **results** directory!

From the **results** directory, type `python2 repro_all.py` to reproduce all
results in the paper.

Expected time: Overnight

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
   Note that we do NOT re-run the latency experiments for 6.2, since these are graphical
   tests and our artifact is a virtual machine with limited graphics capabilities.
   You can re-generate the graphs from our original data, though (see point 5 below).

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

This script executes the JetStream2 benchmark suite for both versions of
Firefox and produces an itemized graph comparing the results. For display
purposes, the individual benchmarks are sorted based on score. This may cause
the order to differ slightly from the paper, because these benchmarks have a
tendency to be noisy. On the topic of noise, any large differences in performance
between the two should steady out on further runs.

** Note: We do not reproduce the latency numbers but the graph can be built **

The results of the latency test can be found in
`~/proofmonkey-gecko-dev[-original]/testing/mozharness/build/local.json`, and
the graph can be built by running `firefox-latency.py` in the `results`
directory.
