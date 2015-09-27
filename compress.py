
import sys
import os
import re

#
# compress.py: Compress expanded hostlists. 

def fst(x):
	return x[0]

def snd(x):
	return x[1]

def splitListByElementSize(intList):
	subLists = [[intList[0]]]
	for x in intList[1:]:
		if len(x) == len(subLists[-1][0]):
			subLists[-1].append(x)
		else:
			subLists.append([x])

	return subLists

def reduceLists(listOfLists):
	combinedList = []
	for x in listOfLists:
		combinedList.extend(x)

	return combinedList


# FIXME This algorithm produces incorrect results when more than
#       one integer range is present in the names.

def compressSublist(subList):
	intList = map(int, subList)

	ranges = []

	n = len(subList[0])

	u = (intList[0],)
	k = 1

	for x in intList[1:] + [None]:
		if x and (x == fst(u) + k):
			u = (fst(u), x)
			k = k + 1
		else:
			ranges.append(u)
			u = (x,)
			k = 1

	ranges = [tuple([("%%0%dd" % n) % z for z in list(x)]) for x in ranges]

	u = reduceLists([list(x) for x in ranges])

	if 1 == len(u):
		return u[0]

	k = 0
	for i in range(n):
		if 0 == len([x for x in u if x[i] != u[0][i]]):
			k = i + 1

	ranges = ["-".join([z[k:] for z in list(x)]) for x in ranges]

	return u[0][:k] + "[" + ",".join(ranges) + "]"

def compressInt(intList):
	subLists = splitListByElementSize(intList)

	return ",".join(map(compressSublist, subLists))

def splitNodeName(node):
	x = re.match(r'([^0-9]*)([0-9]*)(.*)', node)

	u, v, z = x.group(1), x.group(2), x.group(3)

	p = []
	if len(u) > 0:
		p += [('c', u)]
	if len(v) > 0:
		p += [('i', v)]
	if len(z) > 0:
		p += splitNodeName(z)

	return tuple(p)

def nodeToKey(node):
	k = []
	for x in node:
		if 'c' == fst(x):
			k.append(snd(x))
		if 'i' == fst(x):
			k.append("%d" % len(snd(x)))

	return tuple(k)

def compressx(k, nodeList):
	i = [j for (j, h) in enumerate(k) if re.match(r'[0-9]+', h)]
	p = [tuple([snd(x[j]) for j in i]) for x in nodeList]

	k = list(k)
	for j in range(len(i)):
		k[i[j]] = compressInt([x[j] for x in p])

	return "".join(k)

def compress(nodes):
	# FIXME Sorting the node names allows to reach higher compression rates
	#       but in some circumstances it might not be desirable to change the
	#	order.

	nodes = sorted(map(splitNodeName, nodes), key = lambda z: z[0])

	d = {}
	for k, n in zip(map(nodeToKey, nodes), nodes):
		d[k] = d.get(k, []) + [n]

	return ",".join([compressx(k, d[k]) for k in d.keys()])

print(compress(sys.argv[1:]))

