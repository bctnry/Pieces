import random

__doc__ = '''
Please confuse me.
(c) sebastian lin. 2018
'''

def please_confuse_me(source):
    listing = source.split('\n')
    correspondence = {}
    correspondence2 = []
    header = []
    for i, line in enumerate(listing):
        line = line.strip()
        if line[0] == '#':
            header.append(line)
            continue
        current_macro_id = random.randint(10000, 99999)
        while current_macro_id in correspondence:
            current_macro_id = random.randint(10000, 99999)
        correspondence[current_macro_id] = line
        correspondence2.append(current_macro_id)
    macro_id_list = list(correspondence.keys())
    random.shuffle(macro_id_list)
    return ''.join(
        [x + '\n' for x in header] + \
        [''.join(['#define ', ('_'+str(x)), ' ', correspondence[x], '\n']) for x in macro_id_list] + \
        ['\n'.join(['_' + str(x) for x in correspondence2])]
    )

        
