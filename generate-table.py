#!/usr/bin/python3
"""Upload data to imgur if it's changed.

Usage:
  generate-table.py
"""
from docopt import docopt
from collections import defaultdict
import os
import json
import subprocess

def gen(name, md5):
    subprocess.check_output(["convert", name, "-resize", "x200", "-unsharp", "0x1", "/tmp/a.png"])
    r = subprocess.check_output(["./imgur.sh", "/tmp/a.png"]).strip().split()
    return {'md5': md5, 'link': r[0].decode("utf-8"), 'deleteLink': r[3].decode("utf-8")}

def doit():
    data = json.loads(open('uploaded-figures.json').read())
    for img in os.listdir("imgs"):
        img = 'imgs/' +img
        md5 = subprocess.check_output(["/usr/bin/md5sum", img]).strip().split()[0].decode("utf-8")
        if img in data and data[img]['md5'] == md5:
            pass
        else:
            data[img] = gen(img, md5)
            print(data[img])
    json.dump(data, open('uploaded-figures.json', 'w'))

def links():
    data = json.loads(open('uploaded-figures.json').read())
    for k, v in data.items():
        label = os.path.splitext(os.path.basename(k))[0]
        print('[img_%s]: %s "%s"' % (label, v['link'], label))
        fnName = subprocess.check_output(["/bin/grep", "-n", '"' + label + '"', "test/Spec.hs"]).split()[4].decode("utf-8")
        line = subprocess.check_output(["/bin/grep", "-ne", "^" + fnName + ' ', "test/Spec.hs"]).decode("utf-8").split(':')[0]
        print("[url_%s]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L%s" % (label, line))
    for k, v in data.items():
        label = os.path.splitext(os.path.basename(k))[0]
        print("[![%s][img_%s]][url_%s]" % (label, label, label))
    # "| %s | %s | %s |"
# | col 2 is      | centered      |   $12 |
# | zebra stripes | are neat      |    $1 |
    
# [You can use numbers for reference-style link definitions][1]
# ![alt text][logo]
# [![alt text][logo]][1]
    # '[1]: http://slashdot.org'
    # '[logo]: https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 2"'

if __name__ == '__main__':
    arguments = docopt(__doc__, version='0.1')
    doit()
    links()
