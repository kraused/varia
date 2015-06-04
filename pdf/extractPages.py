
#
# extractPages.py: Extract a selection of pages from a file
#                  Usage: python2 extractPages.py <input pdf file> <page range> <output file>
#                  The page range is a comma seperated list of pages that also may contain
#                  ranges in the form start-end. Example: 1,5,2-3
#                  The pages are removed from the input file and the file is modified in place.
#

import os
import sys
import subprocess
import re
import socket
import hashlib
import select
import time


#
# Create a process and return the handle
def createProcess(argv):
	p = subprocess.Popen(argv,
			     stdout = subprocess.PIPE,
			     stderr = subprocess.PIPE)

	return p

#
# Wait for termination of a process.
def waitForProcess(p):
	o, e = p.communicate()
	x = p.wait()

	return (x, o, e)

#
# Mirror stdout and sterr of a process and wait for it to finish
def accompanyProcess(p):
	o = ""
	e = ""

	while 1:
		z, _, _ = select.select([p.stdout, p.stderr], [], [], 0)
		for x in z:
			u = os.read(x.fileno(), 512)
			if p.stdout == x:
				o += u
				sys.stdout.write(u)
			if p.stderr == x:
				e += u
				sys.stderr.write(u)

		x = p.poll()
		if None != x:
			u = p.stdout.read()
			o += u
			sys.stdout.write(u)
			u = p.stderr.read()
			e += u
			sys.stderr.write(u)

			return (x, o, e)

	# Should never happen
	return None

#
# Create a subprocess and wait for it to exit. Return the exit
# code and standard output/exit as a triplet.
def createProcessAndWait(argv):
	return waitForProcess(createProcess(argv))

#
# Variant of createProcessAndWait which does not return the exit code but
# raises an exception if the command failed.
def createProcessAndWait_(argv):
	x, o, e = createProcessAndWait(argv)
	if x:
		raise Exception("Command '%s' failed with exit code %d." % (argv, x))

	return o, e

#
# Create a subprocess and wait for it to exit. Mirror stdout and stderr
# while the process is running. Return the exit code and standard 
# output/exit as a triplet.
def createProcessAndAccompany(argv):
	return accompanyProcess(createProcess(argv))


#
# Create a unique string that can be used, e.g., for the name of a temporary directory
def uniqueString():
	x = hashlib.md5()
	x.update("%s-%0d" % (socket.gethostname(), os.getpid()))
	return x.hexdigest()[:8]

#
# Split the comma-separated list of tickets and expand ranges.
def expand(numbers):
	# Replaces ([0-9]+)-([0-9]+) by a comma separted list
	def match1(matchobj):
		tmp0, tmp1 = matchobj.group(0).split('-')
		
		if int(tmp0) > int(tmp1):
			raise Exception("Invalid ticket list")
		
		tmp2 = range(int(tmp0), int(tmp1) + 1)
		# slurm completely ignores len(tmp1) and we shall follow
		# that example.
		fmt = "%%0%dd" % len(tmp0)
		return ",".join([fmt % z for z in tmp2])
	
	return re.sub(r'([0-9]+)-([0-9]+)', match1, numbers).split(",")

#
def main(argv):
	now = time.strftime("%Y-%m-%dT%H%M%S", time.localtime())

	tmpdir = "/tmp/%s-%s" % (now, uniqueString())

	os.mkdir(tmpdir)

	o, e = createProcessAndWait_(["/usr/bin/gs", "-sDEVICE=pdfwrite", "-dSAFER", "-o", tmpdir + "/%d.pdf", sys.argv[1]])
	sys.stdout.write(o)
	sys.stderr.write(e)

	n = len(os.listdir(tmpdir))
	pages = map(int, expand(sys.argv[2]))

	fileList = [tmpdir + "/%d.pdf" % x for x in pages]

	x, o, e = createProcessAndWait(["/usr/bin/gs", "-sDEVICE=pdfwrite", "-dSAFER", "-o", sys.argv[3]] + fileList)
	sys.stdout.write(o)
	sys.stderr.write(e)

	fileList = [tmpdir + "/%d.pdf" % x for x in range(1, n + 1) if (not x in pages)]

	x, o, e = createProcessAndWait(["/usr/bin/gs", "-sDEVICE=pdfwrite", "-dSAFER", "-o", sys.argv[1]] + fileList)
	sys.stdout.write(o)
	sys.stderr.write(e)

	return x

main(sys.argv)

