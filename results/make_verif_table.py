import os

# Make the verification times table. This script will
# (1) Run `stack test,` which calls all of the verification routines
# (2) Parse that output into a Latex table that you can copy into the paper
# (3) Turn the chart into a PDF that you can compare to the one in the original paper

# extract the strings into numbers rounded correctly
# clip the '(' off the lefthand side, the "s)" off the righthand side
# then round to zero decimal places
def my_round(number): return int(round(float(number[1:len(number)-2]), 0))

# Parse the file into a map that we can then make into a pretty latex picture 
def parse_file(fname):
    with open(fname) as f:
        lines = f.readlines()
        operator    = None
        test_number = 0
        verif_results     = {}
        for i, line in enumerate(lines):
            words = line.split()
            if not words: continue
            # a new operator to verify 
            if words[0] == "XX":
                operator = words[1]
                verif_results[operator] = {}
                test_number = 0
            # a verification condition for the current operator
            if words[0] == "XXX":
                verification_result = "\oo"
                # verification succeeded
                if "OK" in words: verification_result = my_round(words[len(words) - 1])
                if "Failed to verify" in lines[i+1]: verification_result = "X"
                verif_results[operator][test_number] = verification_result
                test_number += 1
    return verif_results

def tex_cmd(cmd, contents): return '\\' + cmd + "{" + contents + "}"

start_tex = [ "\\documentclass{article}\n",
              "\\usepackage{booktabs}\n",
              "\usepackage[landscape]{geometry}\n",
              "\\begin{document}\n",
              "\\newcommand\oo{$\infty$}\n",
              "\\begin{figure}[t]\n",
              "\\small\n",
              "\\centering\n",
              "\\begin{tabular}{lccccccccc|cccc|c} \\toprule\n",
              "Operation &    \\textbf{R1} & \\textbf{R2} & \\textbf{R3} & \\textbf{R4} & \\textbf{R5.i32} & \\textbf{R5.double} & \\textbf{R6.i32} & \\textbf{R6.double} & \\textbf{R7} & \\textbf{W1} & \\textbf{W2} & \\textbf{W3} & \\textbf{W4} & \\textbf{Undef} \\\\ \n",
              "\\midrule\n" ]

end_tex = [ "\\end{tabular}\n",
            "\\vspace*{-3.5mm}\n",
            "\\caption{Reproduced table}\n", 
            "\\end{figure}\n",
            "\\end{document}\n" ]

def pr(operator, m):
    line = tex_cmd("texttt", operator)
    results = m[operator]
    for i in range(14):
            # all VCs apply
            if len(results) == 14: line = line + " & " + str(results[i])
            # just three VCs apply
            elif i == 4:  line = line + " & " + str(results[0])
            elif i == 6:  line = line + " & " + str(results[1])
            elif i == 13: line = line + " & " + str(results[2])
            else:         line = line + " & - "
    line = line + " \\\\\n"
    return line

def make_table(v):
    inf = "\\oo"
    all_lines = []
    all_lines.append(pr("Add", v)) 
    all_lines.append(pr("Sub", v))
    all_lines.append(pr("And", v)) 
    all_lines.append(pr("Or", v))
    all_lines.append(pr("Xor", v))
    all_lines.append(pr("Not", v))
    all_lines.append(pr("Mul", v))
    all_lines.append(pr("Lsh", v))
    all_lines.append(pr("Rsh", v))
    all_lines.append(pr("Ursh", v))
    all_lines.append(pr("Lsh'", v))
    all_lines.append(pr("Rsh'", v))
    all_lines.append(pr("Ursh'", v))
    all_lines.append(pr("Abs", v))
    all_lines.append(pr("Min", v))
    all_lines.append(pr("Max", v))
    all_lines.append(pr("Floor", v))
    all_lines.append(pr("Ceil", v))
    all_lines.append(pr("Sign", v))
    with open("verify_table.tex", 'w') as f:
        f.writelines(start_tex)
        f.writelines(all_lines)
        f.writelines(end_tex)

print("About to run all verification routines and pipe result to file")    
cmd = "stack test --ta '-p Verification' > verif_file.txt"
os.system(cmd)
print("Done verifying. About to generate a table of the results")
results = parse_file("verif_file.txt")
make_table(results)
# now make the pdf
os.system("pdflatex verify_table.tex")





