import os
import re

RESULT = 0

# find_print_call
# find all `print()` calls in all .py scripts under a specific directory.

SKIPPED_FILE = ['find_print_call.py']
REGEX_PRINT_CALL = re.compile(r"print\(")

def check(p):
    global RESULT
    for i in os.scandir(path=p):
        if i in SKIPPED_FILE:
            continue
        if i.path.endswith('.py') and i.is_file():
            with open(i, 'r') as f:
                fstr = f.readlines()
            for lineno, line in enumerate(fstr, 1):
                objs = REGEX_PRINT_CALL.findall(line)
                for mobj in objs:
                    print(f'In file {i} line {lineno}')
            
        elif i.is_dir():
            check(i.path)

check('.')
