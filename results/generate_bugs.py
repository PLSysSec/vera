import os

print("About to run verification on buggy operators")
print("Intersect")
cmd = "stack test --ta '-p Broken_intersect' > bug_examples.txt"
os.system(cmd)
print("Ceil")
cmd = "stack test --ta '-p Broken_ceil' >> bug_examples.txt"
os.system(cmd)
print("Wrote buggy examples to bug_examples.txt")

