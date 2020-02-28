import os

print("About to run verification on buggy operators")
print("Buggy Intersect")
cmd = "stack test --ta '-p Broken_intersect' >& bug_examples.txt"
os.system(cmd)
print("Buggy Ceil")
cmd = "stack test --ta '-p Broken_ceil' &>> bug_examples.txt"
os.system(cmd)
print("---> Wrote buggy examples to results/bug_examples.txt")

