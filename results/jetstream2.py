import json
import os
import subprocess

def extract_json(js_output):
    json_start = js_output.index('{"JetStream2.0"')
    json_text = js_output[json_start:]
    return json.loads(json_text)

def get_scores(obj):
    return {k:v["metrics"]["Score"]["current"][0] for k,v in obj["JetStream2.0"]["tests"].items()}

def mk_csv(scores_old, scores_new):
    out = "Test,Vanilla,Verified\n"
    order = sorted(scores_old.items(), key=lambda tup: tup[1])
    for k,_ in order:
       out += '{},{},{}\n'.format(k, scores_old[k], scores_new[k])
    return out

with open('../JetStream2/output.txt', 'r') as file:
    owd = os.getcwd()
    try:
        os.chdir("../JetStream2")
        print("Running jetstream on original firefox")
        old = subprocess.check_output(['../../proofmonkey-gecko-dev-original/opt-build/js/src/js', '../JetStream2/ff-cli.js'])
        print("Running jetstream on verified firefox")
        new = subprocess.check_output(['../../proofmonkey-gecko-dev/opt-build/js/src/js', '../JetStream2/ff-cli.js'])
    finally:
        os.chdir(owd)
    old_data = extract_json(old)
    old_scores = get_scores(old_data)
    new_data = extract_json(new)
    new_scores = get_scores(new_data)
    with open('./jetstream2.csv', 'w') as outfile:
        # TODO actually compare
        outfile.write(mk_csv(old_scores, new_scores))

os.system('gnuplot ../JetStream2/graph_comp.gnuplot')

