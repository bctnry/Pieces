import os

RESULT = 0

# countlines
# count lines of all .py scripts under a specific directory (recursively)
# ignore blank lines (lines that contains nothing or only whitespaces)

SKIPPED_FILE = ['countlines.py']

def check(p):
    global RESULT
    for i in os.scandir(path=p):
        if i in SKIPPED_FILE:
            continue
        if i.path.endswith('.py') and i.is_file():
            with open(i, 'r') as f:
                RESULT += len([x for x in f.readlines() if x.strip()])
        elif i.is_dir():
            check(i.path)

check('.')

print(RESULT)
