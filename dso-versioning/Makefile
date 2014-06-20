
CC = gcc

.PHONY: test

default: all

help:
	@echo " "
	@echo " make		Build the shared libraries and executables"
	@echo " make help	Print this message"
	@echo " make test	Execute the exectuables with different LD_LIBRARY_PATHs"
	@echo " "

all:
	mkdir 1
	$(CC) -shared -o 1/libdso.so dso1.c -fPIC
	mkdir 2
	$(CC) -shared -o 2/libdso.so dso2.c -fPIC -Wl,--version-script=dso2.map
	mkdir 3
	$(CC) -shared -o 3/libdso.so dso3.c -fPIC -Wl,--version-script=dso3.map
	$(CC) -o exe1 exe.c -L1 -ldso -Wl,-z,defs
	$(CC) -o exe2 exe.c -L2 -ldso -Wl,-z,defs
	$(CC) -o exe3 exe.c -L3 -ldso -Wl,-z,defs

test:
	-LD_LIBRARY_PATH=$(PWD)/1:$(LD_LIBRARY_PATH) ./exe1
	-LD_LIBRARY_PATH=$(PWD)/2:$(LD_LIBRARY_PATH) ./exe1
	-LD_LIBRARY_PATH=$(PWD)/3:$(LD_LIBRARY_PATH) ./exe1
	-LD_LIBRARY_PATH=$(PWD)/1:$(LD_LIBRARY_PATH) ./exe2
	-LD_LIBRARY_PATH=$(PWD)/2:$(LD_LIBRARY_PATH) ./exe2
	-LD_LIBRARY_PATH=$(PWD)/3:$(LD_LIBRARY_PATH) ./exe2
	-LD_LIBRARY_PATH=$(PWD)/1:$(LD_LIBRARY_PATH) ./exe3
	-LD_LIBRARY_PATH=$(PWD)/2:$(LD_LIBRARY_PATH) ./exe3
	-LD_LIBRARY_PATH=$(PWD)/3:$(LD_LIBRARY_PATH) ./exe3

clean:
	-rm 1/libdso.so 2/libdso.so 3/libdso.so 
	-rmdir 1
	-rmdir 2
	-rmdir 3
	-rm exe1 exe2 exe3

