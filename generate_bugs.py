import os

print("About to run verification on buggy operators")
cmd = "stack test --ta '-p Bugs' > bug_examples.txt"
os.system(cmd)
print("Wrote buggy examples to bug_examples.txt")

