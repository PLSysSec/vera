import json

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

with open('output.txt', 'r') as file:
    data = extract_json(file.read())
    scores = get_scores(data)
    print(mk_csv(scores, scores))

