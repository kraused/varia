
import sys
import os
import re

#
# expand.py: Expand compressed hostlists. Try: python expand.py a[0-2,7]b[1-4,8][3-4][5-6],c[3-4,6]d,f,g,h

#
# Split the nodelist outside of [] brackets
def split(nodelist):
	parts = []

	s = 0
	p = ""

	for x in re.split(r'([,\[\]])', nodelist):
		if '[' == x: s = 1
		if ']' == x: s = 0

		if ',' == x and 0 == s:
			parts.append(p)
			p = ""
			continue

		p += x

	if len(p) > 0: parts.append(p)

	return parts

#
# Expand a part of the nodelist
def expandone(nodelist):
	# Replaces ([0-9]+)-([0-9]+) by a comma separted list
	def match1(matchobj):
		tmp0, tmp1 = matchobj.group(0).split('-')

		if int(tmp0) > int(tmp1):
			raise Exception("Invalid hostlist")

		tmp2 = range(int(tmp0), int(tmp1) + 1)
		# slurm completely ignores len(tmp1) and we shall follow
		# that example.
		fmt = "%%0%dd" % len(tmp0)
		return ",".join([fmt % z for z in tmp2])

	def match2(matchobj):
		tmp = matchobj.group(0)
		return re.sub(r'([0-9]+)-([0-9]+)', match1, tmp)

	def match3(matchobj):
		tmp0, tmp1, tmp2 = matchobj.group(1), matchobj.group(2), matchobj.group(3)
		return ",".join([tmp0 + z + tmp2 for z in tmp1.split(",")])

	if not re.match(r'.*\[.*\].*', nodelist):	# Anything to expand?
		return [nodelist]

	return expand(re.sub(r'(.*)\[([0-9,]+)\](.*)', match3, \
	                     re.sub(r'\[([0-9,-]+)\]', match2, nodelist, count = 1), count = 1))

def expand(nodelist):
	return sum([expandone(p) for p in split(nodelist)], [])

print("\n".join(expand(sys.argv[1])))

