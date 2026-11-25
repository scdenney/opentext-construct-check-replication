import sys, re; sys.path.insert(0, "/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/bfe7aca0-b2db-41ad-bf8b-6a8c26510331/scratchpad/rec2")
from lib import CALLS
agent = sys.argv[1]; pat = re.compile(sys.argv[2]) if len(sys.argv) > 2 else re.compile(r"sed -i|open\([^)]*['\"]w|cat >|perl -pi|\.write\(")
for i, x in enumerate(CALLS):
    if not x["owner"].startswith("agent-" + agent) and not x["owner"].startswith(agent): continue
    if x["name"] == "Bash" and pat.search(x["inp"]["command"]):
        print(f"######## [{i}] {x['ts'][11:19]} err={x['err']}\n{x['inp']['command']}\n")
    elif x["name"] == "Edit":
        print(f"######## [{i}] {x['ts'][11:19]} EDIT err={x['err']} {x['inp']['file_path'].split('/')[-1]}\n--- OLD:\n{x['inp']['old_string']}\n--- NEW:\n{x['inp']['new_string']}\n")
