
#include <stdio.h>

void dso_function_version_1()
{
	printf(" This is dso_function_version_1() in libdso3.so\n");
}
asm(".symver dso_function_version_1,dso_function@VERS_1.0");

void dso_function_version_2()
{
	printf(" This is dso_function_version_2() in libdso3.so\n");
}

asm(".symver dso_function_version_2,dso_function@@VERS_2.0");

