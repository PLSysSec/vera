import json
import os

def extract_json(js_output):
    json_start = js_output.index('{"JetStream2.0"')
    json_text = js_output[json_start:]
    return json.loads(json_text)

def get_scores(obj):
    return {k:v["metrics"]["Score"]["current"][0] for k,v in obj["JetStream2.0"]["tests"].items()}

def mk_csv(scores_old, scores_new):
    out = "Test,Vanilla,Verified\n"
    for k in scores_old.keys():
       out += '{},{},{}\n'.format(k, scores_old[k], scores_new[k])
    return out

with open('../JetStream2/output.txt', 'r') as file:
    data = extract_json(file.read())
    scores = get_scores(data)
    with open('./jetstream2.csv', 'w') as outfile:
        # TODO actually compare
        outfile.write(mk_csv(scores, scores))

os.system('gnuplot ../JetStream2/graph_comp.gnuplot')

