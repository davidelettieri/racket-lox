import glob
import re

def replaceLine(m):
    return f"[{m.group(1)} {int(m.group(2))+1}]"

def addDecimal(m):
    return f"// expect: {m.group(1)}.0\n"

print("Searching files...")
for f in glob.glob('craftinginterpreters/test/**/*.lox', recursive=True):
    print("Updating",f)
    with open(f, "r+", encoding= "UTF8") as h:
        content = h.read()
        updatedContent = content
        updated = False
        matches = re.search(r"// expect: (-?\d+)\n", content)
        if matches is not None:
            updatedContent = re.sub(r"// expect: (-?\d+)\n", addDecimal, content)
            updated = True
        if not updatedContent.startswith("#lang"):
            h.truncate(0)
            h.seek(0,0)
            updatedContent = re.sub(r"\[(java line|line) (\d+)\]", replaceLine, updatedContent)
            h.write("#lang racket-lox\n" + updatedContent)
        elif updated:
            h.truncate(0)
            h.seek(0,0)
            h.write(updatedContent)

    print("Update complete for",f)
