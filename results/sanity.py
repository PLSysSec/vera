import os
print("Running sanity checks")
os.system("stack test --ta '-p Sanity' &> verif_sanity.txt")
print("---> Wrote output to results/verify_sanity.txt")
