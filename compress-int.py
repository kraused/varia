
import sys
import re

#
# compress-int.py: Compress an integer lists. Try: python compress.py 001 002 003 1 2 3 4

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

def compressSublist(subList):
	def printRange(x, n):
		return "-".join([("%%0%dd" % n) % z for z in list(x)])

	intList = map(int, subList)

	ranges = []

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

	return ",".join([printRange(x, n) for x, n in zip(ranges, [len(subList[0])]*len(ranges))])

def compress(intList):
	subLists = splitListByElementSize(intList)

	return ",".join(map(compressSublist, subLists))

print(compress(sys.argv[1:]))

