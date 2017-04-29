
import sys
import os
import struct
import gzip
import StringIO

def fst(x):
	return x[0]

def snd(x):
	return x[1]

def getValueOrDefault(x, idx, default):
	if idx >= len(x):
		return default
	return x[idx]

def uncompress(inputData):
	outputData = None

	if inputData.startswith("\xfd7zXZ\x00"):
		import subprocess
		outputData, _ = subprocess.Popen(["/usr/bin/unxz"], stdin = subprocess.PIPE, stdout = subprocess.PIPE).communicate(input = inputData)
	if inputData.startswith("\x1f\x8b\x08\x00"):
		outputData = gzip.GzipFile(fileobj = StringIO.StringIO(inputData)).read()

	# No suitable compression algorithm found?
	assert outputData

	return outputData

def extractString(buf):
	string = ""
	for b in buf:
		if '\0' == b:
			break
		string += b

	return string

UCHAR	= 1
CHAR	= 2
STRING	= 3
SHORT	= 4
INT	= 5

LTENDIAN = 0
BGENDIAN = 1
NATIVE   = 2
NETWORK  = 3

class Struct:
	def __init__(self, members, order):
		self.__memberNames = [fst(x)                     for x in members]
		self.__memberTypes = [snd(x)                     for x in members]
		self.__memberLens  = [getValueOrDefault(x, 2, 1) for x in members]
		self.__order       = order

		self.__struct = struct.Struct(self.__generateStructFmt())

	def __len__(self):
		return sum([self.__typeLen(tp)*n for tp, n in zip(self.__memberTypes, self.__memberLens)])

	def __typeLen(self, tp):
		return {UCHAR: 1, CHAR: 1, STRING: 1, SHORT: 2, INT: 4}[tp]

	def __typeChar(self, tp):
		return {UCHAR: "B", CHAR: "c", STRING: "s", SHORT: "h", INT: "i"}[tp]

	def __generateStructFmt(self):
		fmt = {NATIVE: "@", NETWORK: "!", LTENDIAN: "<", BGENDIAN: ">"}[self.__order]

		for name, tp, n in zip(self.__memberNames, self.__memberTypes, self.__memberLens):
			if n > 1:
				fmt += ("%d" % n)
			fmt += self.__typeChar(tp)

		return fmt

	def parse(self, buf):
		data = self.__struct.unpack(buf[:len(self)])

		values = {}

		i = 0		
		for name, tp, n in zip(self.__memberNames, self.__memberTypes, self.__memberLens):
			if 1 == n:
				values[name] = data[i]; i += 1
			else:
				if STRING == tp:
					values[name] = data[i]; i += 1
				else:
					values[name] = tuple(data[i:(i+n)]); i += n

		return values

class Buffer:
	def __init__(self, buf, pos = 0):
		self.__buf = buf
		self.__pos = pos

	def pointer(self):
		return self.__buf[self.__pos:]

	def advance(self, n):
		self.__pos += n

	def align(self, n):
		self.__pos = (self.__pos + (n - 1)) & (~(n - 1))

	def slice(self, n):
		return Buffer(self.__buf, (self.__pos + n))

rpmTagEnum = {
	"HEADERIMAGE" 			: 61,
	"HEADERSIGNATURES" 		: 62,
	"HEADERIMMUTABLE" 		: 63,
	"HEADERREGIONS" 		: 64,
	"HEADERI18NTABLE" 		: 100,
	"SIG_BASE"			: 256,
	"SIGSIZE"			: 256 + 1,
	"SIGLEMD5_1"			: 256 + 2,
	"SIGPGP"			: 256 + 3,
	"SIGLEMD5_2"			: 256 + 4,
	"SIGMD5"			: 256 + 5,
	"SIGGPG"			: 256 + 6,
	"SIGPGP5"			: 256 + 7,
	"BADSHA1_1"			: 256 + 8,
	"BADSHA1_2"			: 256 + 9,
	"PUBKEYS"			: 256 + 10,
	"DSAHEADER"			: 256 + 11,
	"RSAHEADER"			: 256 + 12,
	"SHA1HEADER"			: 256 + 13,
	"LONGSIGSIZE"			: 256 + 14,
	"LONGARCHIVESIZE"		: 256 + 15,
	"NAME"				: 1000,
	"VERSION"			: 1001,
	"RELEASE"			: 1002,
	"EPOCH"				: 1003,
	"SUMMARY"			: 1004,
	"DESCRIPTION"			: 1005,
	"BUILDTIME"			: 1006,
	"BUILDHOST"			: 1007,
	"INSTALLTIME"			: 1008,
	"SIZE"				: 1009,
	"DISTRIBUTION"			: 1010,
	"VENDOR"			: 1011,
	"GIF"				: 1012,
	"XPM"				: 1013,
	"LICENSE"			: 1014,
	"PACKAGER"			: 1015,
	"GROUP"				: 1016,
	"CHANGELOG"			: 1017,
	"SOURCE"			: 1018,
	"PATCH"				: 1019,
	"URL"				: 1020,
	"OS"				: 1021,
	"ARCH"				: 1022,
	"PREIN"				: 1023,
	"POSTIN"			: 1024,
	"PREUN"				: 1025,
	"POSTUN"			: 1026,
	"OLDFILENAMES"			: 1027,
	"FILESIZES"			: 1028,
	"FILESTATES"			: 1029,
	"FILEMODES"			: 1030,
	"FILEUIDS"			: 1031,
	"FILEGIDS"			: 1032,
	"FILERDEVS"			: 1033,
	"FILEMTIMES"			: 1034,
	"FILEDIGESTS"			: 1035,
	"FILELINKTOS"			: 1036,
	"FILEFLAGS"			: 1037,
	"ROOT"				: 1038,
	"FILEUSERNAME"			: 1039,
	"FILEGROUPNAME"			: 1040,
	"EXCLUDE"			: 1041,
	"EXCLUSIVE"			: 1042,
	"ICON"				: 1043,
	"SOURCERPM"			: 1044,
	"FILEVERIFYFLAGS"		: 1045,
	"ARCHIVESIZE"			: 1046,
	"PROVIDENAME"			: 1047,
	"REQUIREFLAGS"			: 1048,
	"REQUIRENAME"			: 1049,
	"REQUIREVERSION"		: 1050,
	"NOSOURCE"			: 1051,
	"NOPATCH"			: 1052,
	"CONFLICTFLAGS"			: 1053,
	"CONFLICTNAME"			: 1054,
	"CONFLICTVERSION"		: 1055,
	"DEFAULTPREFIX"			: 1056,
	"BUILDROOT"			: 1057,
	"INSTALLPREFIX"			: 1058,
	"EXCLUDEARCH"			: 1059,
	"EXCLUDEOS"			: 1060,
	"EXCLUSIVEARCH"			: 1061,
	"EXCLUSIVEOS"			: 1062,
	"AUTOREQPROV"			: 1063,
	"RPMVERSION"			: 1064,
	"TRIGGERSCRIPTS"		: 1065,
	"TRIGGERNAME"			: 1066,
	"TRIGGERVERSION"		: 1067,
	"TRIGGERFLAGS"			: 1068,
	"TRIGGERINDEX"			: 1069,
	"VERIFYSCRIPT"			: 1079,
	"CHANGELOGTIME"			: 1080,
	"CHANGELOGNAME"			: 1081,
	"CHANGELOGTEXT"			: 1082,
	"BROKENMD5"			: 1083,
	"PREREQ"			: 1084,
	"PREINPROG"			: 1085,
	"POSTINPROG"			: 1086,
	"PREUNPROG"			: 1087,
	"POSTUNPROG"			: 1088,
	"BUILDARCHS"			: 1089,
	"OBSOLETENAME"			: 1090,
	"VERIFYSCRIPTPROG"		: 1091,
	"TRIGGERSCRIPTPROG"		: 1092,
	"DOCDIR"			: 1093,
	"COOKIE"			: 1094,
	"FILEDEVICES"			: 1095,
	"FILEINODES"			: 1096,
	"FILELANGS"			: 1097,
	"PREFIXES"			: 1098,
	"INSTPREFIXES"			: 1099,
	"TRIGGERIN"			: 1100,
	"TRIGGERUN"			: 1101,
	"TRIGGERPOSTUN"			: 1102,
	"AUTOREQ"			: 1103,
	"AUTOPROV"			: 1104,
	"CAPABILITY"			: 1105,
	"SOURCEPACKAGE"			: 1106,
	"OLDORIGFILENAMES"		: 1107,
	"BUILDPREREQ"			: 1108,
	"BUILDREQUIRES"			: 1109,
	"BUILDCONFLICTS"		: 1110,
	"BUILDMACROS"			: 1111,
	"PROVIDEFLAGS"			: 1112,
	"PROVIDEVERSION"		: 1113,
	"OBSOLETEFLAGS"			: 1114,
	"OBSOLETEVERSION"		: 1115,
	"DIRINDEXES"			: 1116,
	"BASENAMES"			: 1117,
	"DIRNAMES"			: 1118,
	"ORIGDIRINDEXES"		: 1119,
	"ORIGBASENAMES"			: 1120,
	"ORIGDIRNAMES"			: 1121,
	"OPTFLAGS"			: 1122,
	"DISTURL"			: 1123,
	"PAYLOADFORMAT"			: 1124,
	"PAYLOADCOMPRESSOR"		: 1125,
	"PAYLOADFLAGS"			: 1126,
	"INSTALLCOLOR"			: 1127,
	"INSTALLTID"			: 1128,
	"REMOVETID"			: 1129,
	"SHA1RHN"			: 1130,
	"RHNPLATFORM"			: 1131,
	"PLATFORM"			: 1132,
	"PATCHESNAME"			: 1133,
	"PATCHESFLAGS"			: 1134,
	"PATCHESVERSION"		: 1135,
	"CACHECTIME"			: 1136,
	"CACHEPKGPATH"			: 1137,
	"CACHEPKGSIZE"			: 1138,
	"CACHEPKGMTIME"			: 1139,
	"FILECOLORS"			: 1140,
	"FILECLASS"			: 1141,
	"CLASSDICT"			: 1142,
	"FILEDEPENDSX"			: 1143,
	"FILEDEPENDSN"			: 1144,
	"DEPENDSDICT"			: 1145,
	"SOURCEPKGID"			: 1146,
	"FILECONTEXTS"			: 1147,
	"FSCONTEXTS"			: 1148,
	"RECONTEXTS"			: 1149,
	"POLICIES"			: 1150,
	"PRETRANS"			: 1151,
	"POSTTRANS"			: 1152,
	"PRETRANSPROG"			: 1153,
	"POSTTRANSPROG"			: 1154,
	"DISTTAG"			: 1155,
	"OLDSUGGESTSNAME"		: 1156,
	"OLDSUGGESTSVERSION"		: 1157,
	"OLDSUGGESTSFLAGS"		: 1158,
	"OLDENHANCESNAME"		: 1159,
	"OLDENHANCESVERSION"		: 1160,
	"OLDENHANCESFLAGS"		: 1161,
	"PRIORITY"			: 1162,
	"CVSID"				: 1163,
	"BLINKPKGID"			: 1164,
	"BLINKHDRID"			: 1165,
	"BLINKNEVRA"			: 1166,
	"FLINKPKGID"			: 1167,
	"FLINKHDRID"			: 1168,
	"FLINKNEVRA"			: 1169,
	"PACKAGEORIGIN"			: 1170,
	"TRIGGERPREIN"			: 1171,
	"BUILDSUGGESTS"			: 1172,
	"BUILDENHANCES"			: 1173,
	"SCRIPTSTATES"			: 1174,
	"SCRIPTMETRICS"			: 1175,
	"BUILDCPUCLOCK"			: 1176,
	"FILEDIGESTALGOS"		: 1177,
	"VARIANTS"			: 1178,
	"XMAJOR"			: 1179,
	"XMINOR"			: 1180,
	"REPOTAG"			: 1181,
	"KEYWORDS"			: 1182,
	"BUILDPLATFORMS"		: 1183,
	"PACKAGECOLOR"			: 1184,
	"PACKAGEPREFCOLOR"		: 1185,
	"XATTRSDICT"			: 1186,
	"FILEXATTRSX"			: 1187,
	"DEPATTRSDICT"			: 1188,
	"CONFLICTATTRSX"		: 1189,
	"OBSOLETEATTRSX"		: 1190,
	"PROVIDEATTRSX"			: 1191,
	"REQUIREATTRSX"			: 1192,
	"BUILDPROVIDES"			: 1193,
	"BUILDOBSOLETES"		: 1194,
	"DBINSTANCE"			: 1195,
	"NVRA"				: 1196,
	"FILENAMES"			: 5000,
	"FILEPROVIDE"			: 5001,
	"FILEREQUIRE"			: 5002,
	"FSNAMES"			: 5003,
	"FSSIZES"			: 5004,
	"TRIGGERCONDS"			: 5005,
	"TRIGGERTYPE"			: 5006,
	"ORIGFILENAMES"			: 5007,
	"LONGFILESIZES"			: 5008,
	"LONGSIZE"			: 5009,
	"FILECAPS"			: 5010,
	"FILEDIGESTALGO"		: 5011,
	"BUGURL"			: 5012,
	"EVR"				: 5013,
	"NVR"				: 5014,
	"NEVR"				: 5015,
	"NEVRA"				: 5016,
	"HEADERCOLOR"			: 5017,
	"VERBOSE"			: 5018,
	"EPOCHNUM"			: 5019,
	"PREINFLAGS"			: 5020,
	"POSTINFLAGS"			: 5021,
	"PREUNFLAGS"			: 5022,
	"POSTUNFLAGS"			: 5023,
	"PRETRANSFLAGS"			: 5024,
	"POSTTRANSFLAGS"		: 5025,
	"VERIFYSCRIPTFLAGS"		: 5026,
	"TRIGGERSCRIPTFLAGS"		: 5027,
	"COLLECTIONS"			: 5029,
	"POLICYNAMES"			: 5030,
	"POLICYTYPES"			: 5031,
	"POLICYTYPESINDEXES"		: 5032,
	"POLICYFLAGS"			: 5033,
	"VCS"				: 5034,
	"ORDERNAME"			: 5035,
	"ORDERVERSION"			: 5036,
	"ORDERFLAGS"			: 5037,
	"MSSFMANIFEST"			: 5038,
	"MSSFDOMAIN"			: 5039,
	"INSTFILENAMES"			: 5040,
	"REQUIRENEVRS"			: 5041,
	"PROVIDENEVRS"			: 5042,
	"OBSOLETENEVRS"			: 5043,
	"CONFLICTNEVRS"			: 5044,
	"FILENLINKS"			: 5045,
	"RECOMMENDNAME"			: 5046,
	"RECOMMENDVERSION"		: 5047,
	"RECOMMENDFLAGS"		: 5048,
	"SUGGESTNAME"			: 5049,
	"SUGGESTVERSION"		: 5050,
	"SUGGESTFLAGS"			: 5051,
	"SUPPLEMENTNAME"		: 5052,
	"SUPPLEMENTVERSION"		: 5053,
	"SUPPLEMENTFLAGS"		: 5054,
	"ENHANCENAME"			: 5055,
	"ENHANCEVERSION"		: 5056,
	"ENHANCEFLAGS"			: 5057,
	"RECOMMENDNEVRS"		: 5058,
	"SUGGESTNEVRS"			: 5059,
	"SUPPLEMENTNEVRS"		: 5060,
	"ENHANCENEVRS"			: 5061,
	"ENCODING"			: 5062,
	"FILETRIGGERIN"			: 5063,
	"FILETRIGGERUN"			: 5064,
	"FILETRIGGERPOSTUN"		: 5065,
	"FILETRIGGERSCRIPTS"		: 5066,
	"FILETRIGGERSCRIPTPROG"		: 5067,
	"FILETRIGGERSCRIPTFLAGS"	: 5068,
	"FILETRIGGERNAME"		: 5069,
	"FILETRIGGERINDEX"		: 5070,
	"FILETRIGGERVERSION"		: 5071,
	"FILETRIGGERFLAGS"		: 5072,
	"TRANSFILETRIGGERIN"		: 5073,
	"TRANSFILETRIGGERUN"		: 5074,
	"TRANSFILETRIGGERPOSTUN"	: 5075,
	"TRANSFILETRIGGERSCRIPTS"	: 5076,
	"TRANSFILETRIGGERSCRIPTPROG"	: 5077,
	"TRANSFILETRIGGERSCRIPTFLAGS"	: 5078,
	"TRANSFILETRIGGERNAME"		: 5079,
	"TRANSFILETRIGGERINDEX"		: 5080,
	"TRANSFILETRIGGERVERSION"	: 5081,
	"TRANSFILETRIGGERFLAGS"		: 5082,
	"REMOVEPATHPOSTFIXES"		: 5083,
	"FILETRIGGERPRIORITIES"		: 5084,
	"TRANSFILETRIGGERPRIORITIES"	: 5085,
	"FILETRIGGERCONDS"		: 5086,
	"FILETRIGGERTYPE"		: 5087,
	"TRANSFILETRIGGERCONDS"		: 5088,
	"TRANSFILETRIGGERTYPE"		: 5089,
	"FILESIGNATURES"		: 5090,
	"FILESIGNATURELENGTH"		: 5091
}

rpmTagTypeEnum = {
	"NULL_TYPE"			: 0,
	"CHAR_TYPE"			: 1,
	"INT8_TYPE"			: 2,
	"INT16_TYPE"			: 3,
	"INT32_TYPE"			: 4,
	"INT64_TYPE"			: 5,
	"STRING_TYPE"			: 6,
	"BIN_TYPE"			: 7,
	"STRING_ARRAY_TYPE"		: 8,
	"I18NSTRING_TYPE"		: 9
}

sigTagEnum = {
	"HEADERSIGNATURES" 	: rpmTagEnum["HEADERSIGNATURES"],
	"SIZE"			: 1000,
	"LEMD5_1"		: 1001,
	"PGP"			: 1002,
	"LEMD5_2"		: 1003,
	"MD5"			: 1004,
	"GPG"			: 1005,
	"PGP5"			: 1006,
	"PAYLOADSIZE"		: 1007,
	"RESERVEDSPACE"		: 1008,
	"BADSHA1_1"		: rpmTagEnum["BADSHA1_1"],
	"BADSHA1_2"		: rpmTagEnum["BADSHA1_2"],
	"SHA1"			: rpmTagEnum["SHA1HEADER"],
	"DSA"			: rpmTagEnum["DSAHEADER"],
	"RSA"			: rpmTagEnum["RSAHEADER"],
	"LONGSIZE"		: rpmTagEnum["LONGSIGSIZE"]
}

rpmTagToName = {}
for k, v in rpmTagEnum.iteritems():
	rpmTagToName[v] = k

rpmTagTypeToName = {}
for k, v in rpmTagTypeEnum.iteritems():
	rpmTagTypeToName[v] = k

sigTagToName = {}
for k, v in sigTagEnum.iteritems():
	sigTagToName[v] = k

leadStruct       = Struct([("magic", UCHAR, 4), ("major", UCHAR), ("minor", UCHAR), ("type", SHORT), ("archnum", SHORT), ("name", STRING, 66), ("osnum", SHORT), ("signatureType", SHORT), ("reserved", CHAR, 16)], NETWORK)
headerStruct     = Struct([("magic", UCHAR, 4), ("reserved", UCHAR, 4), ("nindex", INT), ("hsize", INT)], NETWORK)
headerIdxStruct  = Struct([("tag", INT), ("type", INT), ("offset", INT), ("count", INT)], NETWORK)
cpioHeaderStruct = Struct([("magic", CHAR, 6), ("ino", CHAR, 8), ("mode", CHAR, 8), ("uid", CHAR, 8), ("gid", CHAR, 8), ("nlink", CHAR, 8), ("mtime", CHAR, 8), ("filesize", CHAR, 8), ("devmajor", CHAR, 8), ("devminor", CHAR, 8), ("rdevmajor", CHAR, 8), ("rdevminor", CHAR, 8), ("namesize", CHAR, 8), ("check", CHAR, 8)], NATIVE)

content = Buffer(open(sys.argv[1], "rb").read())

lead = leadStruct.parse(content.pointer())
assert("edabeedb" == "%02x%02x%02x%02x" % lead["magic"])

content.advance(len(leadStruct))
content.align(8)

sig = headerStruct.parse(content.pointer())
content.advance(len(headerStruct))

assert("8eade801" == "%02x%02x%02x%02x" % sig["magic"])

store = content.slice(sig["nindex"]*len(headerIdxStruct))

for i in range(sig["nindex"]):
	record = headerIdxStruct.parse(content.pointer())
	content.advance(len(headerIdxStruct))

	record["tag"]  = sigTagToName[record["tag"]]
	record["type"] = rpmTagTypeToName[record["type"]]

content.advance(sig["hsize"])
content.align(8)

header  = headerStruct.parse(content.pointer())
content.advance(len(headerStruct))

assert("8eade801" == "%02x%02x%02x%02x" % header["magic"])

store = content.slice(header["nindex"]*len(headerIdxStruct))

for i in range(header["nindex"]):
	record = headerIdxStruct.parse(content.pointer())
	content.advance(len(headerIdxStruct))

	record["tag"]  = rpmTagToName[record["tag"]]
	record["type"] = rpmTagTypeToName[record["type"]]

#	if record["type"] in "STRING_TYPE":
#		print(record["tag"], extractString(store.pointer()[record["offset"]:]))

content.advance(header["hsize"])

payload = uncompress(content.pointer())
while 1:
	cpioHeader = cpioHeaderStruct.parse(payload)
	payload    = payload[len(cpioHeaderStruct):]

	assert("070701" == "".join(cpioHeader["magic"]))

	filesize = int("0x" + "".join(cpioHeader["filesize"]), 16)
	namesize = int("0x" + "".join(cpioHeader["namesize"]), 16)

	name    = str(payload[:(namesize - 1)])
	if "TRAILER!!!" == name.strip():
		break

	# Pad to multiple of four
	skip    = ((len(cpioHeaderStruct) + namesize + 3) & (~3)) - len(cpioHeaderStruct)
	payload = payload[skip:]

	path = [name]
	while -1 != path[0].find("/"):
		path = list(os.path.split(path[0])) + path[1:]

	for i in range(1, len(path)):
		p = "/".join(path[:i])
		try:
			os.stat(p)
		except OSError:
			os.mkdir(p)

	with open(name, "w") as f:
		f.write(payload[:filesize])
	print(name)

	skip    = ((filesize + 3) & (~3))
	payload = payload[skip:]

