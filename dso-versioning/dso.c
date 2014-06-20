
#include <stdio.h>

void dso_function_version_1()
{
	printf(" This is version 1 of dso_function().\n");
}

asm(".symver dso_function_version_1,dso_function@VERS_1.0");

void dso_function_version_2()
{
	printf(" This is version 2 of dso_function().\n");
}

asm(".symver dso_function_version_2,dso_function@@VERS_2.0");

