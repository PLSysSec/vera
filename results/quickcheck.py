import os

print("About to run quickcheck on JS operators")
os.system("stack test --ta '-p JS_Fast' &> quickcheck.txt")
print("About to run quickechk on C++ operators")
os.system("stack test --ta '-p Cpp_Fast' &>> quickcheck.txt")
print("---> Wrote quickcheck results to quickcheck.txt")

