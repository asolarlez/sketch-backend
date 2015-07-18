#!/usr/bin/env python

from __future__ import print_function

import cStringIO
import datetime
import fileinput
from optparse import OptionParser
import os
import re
from subprocess import Popen, PIPE
import sys


"""
find DAG, which looks like:

  dag$name{
  ...
  }

"""
def extract_dag(output, findAll=True):
  dags = []
  buf = cStringIO.StringIO()
  flag_in = False
  for line in output:
    if not flag_in and "dag" in line and '{' in line:
      flag_in = True

    if flag_in:
      buf.write(line)

    if flag_in and '}' in line:
      flag_in = False
      dags.append(buf.getvalue())
      buf.close()
      if not findAll: return dags
      buf = cStringIO.StringIO()

  return dags


hole_re = re.compile(r"(H__[\d_]+)")
num_re = re.compile(r"(\d+)")
def sanitize(node):
  m = hole_re.search(node)
  if m:
    return m.group(1)
  m = num_re.search(node)
  if m:
    return m.group(1)

  return node


bops = ['^', '&', '|']
def findParents(txt):
  for bop in bops:
    if bop in txt:
      mother, father = map(lambda x: sanitize(x), txt.split(bop))
      return bop, mother, father

  if "ASSERT" in txt:
    mother = sanitize(txt)
    if mother: return "ASSERT", mother, None

  if '!' in txt:
    mother = sanitize(txt)
    if mother: return '!', mother, None

  return None, None, None



"""
translate to dot
"""
def toDOT(dag):
  buf = cStringIO.StringIO()

  def add_edge(x, y):
    buf.write("  {0} -> {1}".format(x, y) + os.linesep)
    # re-balance the layout
    if x.startswith('H'):
      xy = "{0}{1}".format(x, y)
      buf.write("  {0} [label=\"{0}\", width=.1, style=invis]".format(xy) + os.linesep)
      buf.write("  {0} -> {1} [style=invis]".format(x, xy) + os.linesep)

  for line in dag.splitlines():
    if '{' in line: # starting mark
      buf.write("digraph " + line + os.linesep)
      buf.write("  node [shape=ellipse fontname=\"courier\"]" + os.linesep)

    elif '=' in line: # node with parents
      nid, parents = map(lambda x: x.strip(), line.split('='))
      buf.write("  {0} [label=\"{0}\"]".format(nid) + os.linesep)

      op, mother, father = findParents(parents)
      if mother: add_edge(mother, nid)
      if father: add_edge(father, nid)

    elif '}' in line: # ending mark
      buf.write(line)

    else: # plain node, maybe hole
      buf.write("  {0} [label=\"{0}\"]".format(line) + os.linesep)

  dot = buf.getvalue()
  buf.close()
  return dot


"""
run dot and save it to an image (pdf, png, etc.)
"""
dag_re = re.compile(r"digraph (\S+){")
def toImg(dot, ext="pdf"):
  m = dag_re.search(dot)
  if m:
    dag_name = m.group(1)
  else:
    dag_name = "untitled"
  
  now = datetime.datetime.now().strftime("%H%M%S%f")
  pdf = "{}_{}.{}".format(dag_name, now, ext)

  p = Popen(["dot", "-T"+ext, "-o", pdf], stdin=PIPE, stdout=PIPE, stderr=PIPE)
  out, err = p.communicate(input=dot)
  if out: print(out, file=sys.stdout)
  if err: print(err, file=sys.stderr)


def main():
  parser = OptionParser(usage="usage: %prog [options] sketch_output.txt")
  parser.add_option("--first-only",
    action="store_true", dest="first_only", default=False,
    help="draw the first DAG only")

  (opt, argv) = parser.parse_args()

  if len(argv) < 1:
    output = fileinput.input()
  else:
    output = []
    for arg in argv:
      with open(arg, 'r') as f:
        output.extend(f.readlines())

  dags = extract_dag(output, not opt.first_only)
  for dag in dags:
    toImg(toDOT(dag))


if __name__ == "__main__":
  sys.exit(main())

