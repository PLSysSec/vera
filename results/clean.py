import os

# should probably validate were in the results directory....
os.system("rm *~ &> /dev/null")
os.system("rm *.txt &> /dev/null")
os.system("rm *.aux &> /dev/null")
os.system("rm *.log &> /dev/null")
os.system("rm *.pdf &> /dev/null")
os.system("rm *.csv &> /dev/null")
os.system("rm *.tex &> /dev/null")
print("Cleaned")
