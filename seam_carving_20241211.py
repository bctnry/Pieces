import os
import sys
import math
from PIL import Image

if len(sys.argv) < 4:
    print(f"Usage: python {sys.argv[0]} [new-width] [new-height] [filename]")
im = Image.open(sys.argv[3])

def get_luminance(im, x, y):
    if x < 0 or y < 0 or x >= im.width or y >= im.height: return 0
    return im.getpixel((x, y))
    
def sobel(im):
    v = Image.new('L', (im.width, im.height))
    for y in range(0, im.height):
        for x in range(0, im.width):
            p1 = get_luminance(im, x-1,y-1)
            p2 = get_luminance(im, x,y-1)
            p3 = get_luminance(im, x+1,y-1)
            p4 = get_luminance(im, x-1,y)
            p6 = get_luminance(im, x+1,y)
            p7 = get_luminance(im, x-1,y+1)
            p8 = get_luminance(im, x,y+1)
            p9 = get_luminance(im, x+1,y+1)
            gx = p1 - p3 + 2 * p4 - 2 * p6 + p7 - p9
            gy = p1 + 2 * p2 + p3 - p7 - 2 * p8 - p9
            g = math.sqrt(gx ** 2 + gy ** 2)
            gsc = 0 if g < 0 else 255 if g > 255 else g
            v.putpixel((x, y), int(gsc))
    return v

def shrinkhorizontal(ims):
    im = sobel(ims.convert('L'))
    w = ims.width
    h = ims.height
    d = [[0 for _ in range(0, w)] for _ in range(0, h)]
    for x in range(0, w):
        d[0][x] = im.getpixel((x, 0))
    def get_importance(x, y):
        if x < 0 or y < 0 or x >= w or y >= h: return math.inf
        return d[y][x]
    for y in range(1, h):
        for x in range(0, w):
            v = im.getpixel((x, y))
            p1 = get_importance(x-1, y-1) + v
            p2 = get_importance(x, y-1) + v
            p3 = get_importance(x+1, y-1) + v
            if p1 <= p2 and p1 <= p3: d[y][x] = p1
            elif p2 <= p1 and p2 <= p3: d[y][x] = p2
            elif p3 <= p1 and p3 <= p2: d[y][x] = p3
    min_i = 0
    for x in range(1, w):
        if d[h-1][x] < d[h-1][min_i]: min_i = x
    trail = [min_i]
    for y in range(h-2, -1, -1):
        p1 = get_importance(min_i - 1, y)
        p2 = get_importance(min_i, y)
        p3 = get_importance(min_i + 1, y)
        if p1 <= p2 and p1 <= p3:
            next_i = min_i-1
        elif p2 <= p1 and p2 <= p3:
            next_i = min_i
        elif p3 <= p1 and p3 <= p2:
            next_i = min_i+1
        min_i = next_i
        trail.append(next_i)
    res = Image.new('RGB', (w-1, h))
    trail = list(reversed(trail))
    for y in range(0, h):
        x = 0
        for xc in range(0, w):
            if xc != trail[y]:
                res.putpixel((x, y), ims.getpixel((xc, y)))
                x += 1
    return res

def shrinkmultihorizontal(ims, s):
    im = ims
    for i in range(0, s):
        print(f'{i+1}/{s}...')
        im = shrinkhorizontal(im)
    return im

def shrinkmultivertical(ims, s):
    im = ims.transpose(method=Image.Transpose.ROTATE_90)
    im = shrinkmultihorizontal(im, s)
    im = im.transpose(method=Image.Transpose.ROTATE_270)
    return im


# dw = im.width - int(sys.argv[1])
# dh = im.height - int(sys.argv[2])
dw = int(sys.argv[1])
dh = int(sys.argv[2])

shrinkmultivertical(shrinkmultihorizontal(im, dw), dh).save("result.png")
