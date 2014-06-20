
#include <stdio.h>

void dso_function_version_1()
{
	printf(" This is dso_function_version_1() in libdso1.so\n");
}

extern typeof(dso_function_version_1) \
	dso_function __attribute__((alias("dso_function_version_1"))); 

