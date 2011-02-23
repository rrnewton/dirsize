#!/usr/bin/env python

import os
import sys

def count_dir(dirname):
  o = dirname.count('/')
  counts = {}
  for root, dirs, files in os.walk(dirname, topdown=False):
    total = counts.pop(root, 0)
    fullnames = (os.path.join(root, f) for f in files)
    total += sum(os.path.getsize(f) for f in fullnames if not os.path.islink(f))
    # could append here and sum above to do in a distributed way
    parent = os.path.dirname(root)
    counts[parent] = counts.get(parent, 0) + total
    if dirname == root:
      print "Total for %s: %s bytes" % (os.path.abspath(dirname), total)
    else:
      print "%s %s: %s" % ('+' * (root.count('/') - o), root, total) 

if __name__ == '__main__':
  if len(sys.argv) == 1:
    dirname = os.getcwd()
  elif len(sys.argv) == 2:
    dirname = os.path.normpath(sys.argv[1])
  else:
    print 'Usage: ds <dirname>'
    exit(2)
  if not os.path.exists(dirname):
    print "%s: 0" % (dirname,)
  else:
    count_dir(dirname)

    
        
        
