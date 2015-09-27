
import sys
import re

#
# expand-int.py: Expand compressed integer lists. Try: python expand.py 0-2,7,9-10,5

#
# Split the comma-separated list of integers and expand ranges.
def expand(intList):
	# Replaces ([0-9]+)-([0-9]+) by a comma separted list
	def match1(matchobj):
		tmp0, tmp1 = matchobj.group(0).split('-')

		if int(tmp0) > int(tmp1):
			raise Exception("Invalid integer list")

		tmp2 = range(int(tmp0), int(tmp1) + 1)
		# slurm completely ignores len(tmp1) and we shall follow
		# that example.
		fmt = "%%0%dd" % len(tmp0)
		return ",".join([fmt % z for z in tmp2])

	return re.sub(r'([0-9]+)-([0-9]+)', match1, intList).split(",")

print(" ".join(expand(sys.argv[1])))

