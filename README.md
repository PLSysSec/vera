# VeRA Artifact Evaluation 

Hello, and thanks for evaluating this artifact!

The evaluation has six parts:
(1) Can VeRA prove Firefox range analysis correctness?
(2) Can VeRA proofs catch real correctness bugs?
(3) Are the VeRA proofs correct?
(4) Do the verified routines work correctly in Firefox?
(5) How do the verified routines perform in Firefox?
(6) How hard is it to integrate the verified routines into Firefox?

We are able to reproduce the results from points 1-5, and additionally
reproduce the LOC counts for the different components of the system.

You can use the script WHAT to generate results for all parts of the paper
(expect this to take XXXX).

Alternatively, to generate results for each portion individually, follow
the following instructions.

## (1) Can VeRA prove Firefox range analysis correctness?

The script for generating Figure 8 is called make_verif_table.py
This script calls `stack test,` which verifies each range analysis
operator correct wrt to JavaScript semantics. It will generate a
standalone PDF of the time it took each verification condition
to verify in verify_table.pdf

## (2) Can VeRA proofs catch real correctness bugs?

The script for generating the examples in 6.1's "A new Firefox bug" and
"An old Firefox bug" are in generate_bugs.py
This script runs verification routines for both buggy operators and displays
(1) a counterexample showing that each operator is buggy and (2) the time
it took to generate that example. 

## (3) Are the VeRA proofs correct?

DS

## (4) Do the verified routines work correctly in Firefox?

## (5) How do the verified routines perform in Firefox?


