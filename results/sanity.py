import os
print("Running sanity checks")
os.system("stack test --ta '-p Sanity' &> verify_sanity.txt")
#os.system("./mach jstests test262/harness/nans.js")
print("---> Wrote output to results/verify_sanity.txt")
