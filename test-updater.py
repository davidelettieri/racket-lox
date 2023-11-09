import glob
import re

def replaceLine(m):
    return f"[{m.group(1)} {int(m.group(2))+1}]"

print("Searching files...")
for f in glob.glob('craftinginterpreters/test/**/*.lox', recursive=True):
    print("Updating",f)
    with open(f, "r+") as h:
        content = h.read()
        updatedContent = re.sub(r"\[(java line|line) (\d+)\]", replaceLine, content)
        h.seek(0,0)
        h.write("#lang racket-lox\n" + updatedContent)
    print("Update complete for",f)
    
