import os

print("About to run quickcheck on JS operators for a LONG time")
os.system("stack test --ta '-p JS1000' &> quickcheck.txt")
print("About to run quickechk on C++ operators")
os.system("stack test --ta '-p Cpp1000' &>> quickcheck.txt")
print("---> Wrote quickcheck results to quickcheck.txt")

