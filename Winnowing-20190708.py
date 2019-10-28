# winnowing.
# http://theory.stanford.edu/~aiken/publications/papers/sigmod03.pdf

import io
import sys
import tokenize

# requires `hash_it` to be of type Iter[Tuple[hash, position]]
def winnowing(window_size, hash_it):
    res = []
    window = [(sys.maxsize, -1) for _ in range(window_size)]
    r = 0      # window right end
    min_i = 0  # index of minum hash
    for elem in hash_it:
        r = (r + 1) % window_size
        window[r] = elem
        if min_i == r:
            # the previous minimum is already replaced with next(hash_it).
            # finding the smallest hash value in current window.
            _i = (r - 1) % window_size
            while _i != r:
                if window[_i][0] < window[min_i][0]:
                    min_i = _i
                _i = (_i - 1 + window_size) % window_size
            res.append(window[min_i])
        else:
            if window[r][0] <= window[min_i][0]:
                min_i = r
                res.append(window[min_i])
    return res

def kgram(k, source):
    source = normalize(source)
    return [source[i:i+k]for i, _ in enumerate(source[:-k+1])]

def hash_source(source, k):
    return [(hash(x), i, x) for i, x in enumerate(kgram(k, source))]

def fingerprinting(source, k, w):
    hashed = hash_source(source, k)
    winnowed = winnowing(w, hashed)
    return winnowed

def build_index(fp_list):
    index = {}
    for _docid, fp in enumerate(fp_list):
        for piece in fp:
            h = piece[0]
            if h not in index:
                index[h] = {}
            if _docid not in index[h]:
                index[h][_docid] = []
            index[h][_docid].append(piece)
    return index

def match(fp, index):
    hit = {}
    for piece in fp:
        h = piece[0]
        if h in index:
            for docid in index[h]:
                if docid not in hit:
                    hit[docid] = set()
                hit[docid] |= set(index[h][docid])
    for docid in hit:
        hit[docid] = sorted(list(hit[docid]), key=lambda x: x[1])
    return hit

def calculate(hitmap, fp_list):
    res = {}
    for docid in hitmap:
        res[docid] = len(hitmap[docid])/len(fp_list[docid])
    return res

# only for matching purposes.
# TODO: this might need a LOT of tweaking...
def normalize(source):
    token_list = tokenize.tokenize(io.BytesIO(source.encode('utf-8')).readline)
    # 5 means token.INDENT, 6 means token.DEDENT
    res = ' '.join(['\t' if x.type == 5 else '' if x.type == 6 else x.string for x in token_list][1:])
    return res

def to_tokenlist(source):
    return tokenize.tokenize(io.BytesIO(source.encode('utf-8')).readline)

# currently the relatively safe thresholds are 55%, 80% and 90%, which means
# "somewhat similar", "similar" and "very similar", respectively.

source1 = '''def main():
    print("hel")
    print("lo ")
    print("wor")
    print("ld!")
'''
fp1 = fingerprinting(source1, 3, 5)

source2 = '''def main():
    print("hello world!")
'''
fp2 = fingerprinting(source2, 3, 5)

source3 = '''def main():
    my_str = "hello world!"
    print(my_str)
'''
fp3 = fingerprinting(source3, 3, 5)

source4 = '''def fac(x):
    if x <= 1:
        return 1
    else:
        return x * fac(x - 1)
def main():
    print(fac(5))
'''
fp4 = fingerprinting(source4, 3, 5)
        
source5 = '''def main():
    print(my_str)
    my_str = "hello world!"
'''
fp5 = fingerprinting(source5, 3, 5)
