import os

# Make the verification times table. This script will
# (1) Run `stack test,` which calls all of the verification routines
# (2) Parse that output into a Latex table that you can copy into the paper

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
        for line in lines:
            words = line.split()
            if not words: continue
            # a new operator to verify 
            if words[0] == "XX":
                operator = words[1]
                verif_results[operator] = {}
                test_number = 0
            # a verification condition for the current operator
            if words[0] == "XXX":
                # todo distinguish between verification failure and timeout
                verification_result = "FAIL"
                # verification succeeded
                if "OK" in words: verification_result = my_round(words[len(words) - 1])
                verif_results[operator][test_number] = verification_result
                test_number += 1
    return verif_results

def tex_cmd(cmd, contents): return '\\' + cmd + "{" + contents + "}"
    
def make_table(verif_results):
    inf = "\oo"
    all_lines = []
    for operator, results in verif_results.items():
        line = tex_cmd("texttt", operator)
        for i in range(14):
            if len(results) == 14: line = line + " & " + str(results[i])
            elif i == 4:  line = line + " & " + str(results[0])
            elif i == 6:  line = line + " & " + str(results[1])
            elif i == 13: line = line + " & " + str(results[2])
            else:         line = line + " & - "
        line = line + " \\\\"
        all_lines.append(line)
    print(all_lines)

# cmd = "stack test > verif_file.txt"
# result = os.system(cmd)
# if result > 0:
#     print("Verification failed with error code " + str(result))
#     exit()
results = parse_file("verif_file.txt")
make_table(results)





