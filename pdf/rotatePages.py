
#
# rotatePages.py: Rotate pages by 180 degrees.
#                 Usage: python2 rotatePages.py <pdf file>
#                 The file is modified in place.
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
def main(argv):
	now = time.strftime("%Y-%m-%dT%H%M%S", time.localtime())

	tmpdir = "/tmp/%s-%s" % (now, uniqueString())

	os.mkdir(tmpdir)
	os.mkdir(tmpdir + "/a")
	os.mkdir(tmpdir + "/b")

	o, e = createProcessAndWait_(["/usr/bin/gs", "-sDEVICE=pdfwrite", "-dSAFER", "-o", tmpdir + "/a/%d.pdf", sys.argv[1]])
	sys.stdout.write(o)
	sys.stderr.write(e)

	n = len(os.listdir(tmpdir + "/a"))

	d = os.getcwd()
	os.chdir(tmpdir + "/b")

	for i in range(1, n+1):
		open(tmpdir + "/b/%d.tex" % i, "w").write("""
\\batchmode
\\documentclass[a4paper,]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{pdfpages}

\\begin{document}
\\includepdfmerge[angle=180,fitpaper=true]{%s/a/%d.pdf,-}
\\end{document}
""" % (tmpdir, i))
		
		o, e = createProcessAndWait_(["/opt/TeXLive/20140629/2014/bin/x86_64-linux/pdflatex", tmpdir + "/b/%d.tex" % i])
		sys.stdout.write(o)
		sys.stderr.write(e)

	os.chdir(d)

	fileList = [tmpdir + "/b/%d.pdf" % x for x in range(1, n+1)]

	x, o, e = createProcessAndWait(["/usr/bin/gs", "-sDEVICE=pdfwrite", "-dSAFER", "-o", sys.argv[1]] + fileList)
	sys.stdout.write(o)
	sys.stderr.write(e)

	return x

main(sys.argv)

