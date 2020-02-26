# Script to reproduce all results from the paper

print("About to reproduce the paper results in the 'results' directory")
print("1. Can VeRA prove Firefox range analysis correctness?")
execfile('make_verif_table.py')
print("2. Can VeRA find buggy counterexamples?")
execfile('generate_bugs.py')
print("3. Are the VeRA proofs correct?")
execfile('quickcheck.py')
