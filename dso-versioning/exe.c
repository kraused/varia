
#include <stdio.h>

extern void dso_function();

int main(int argc, char** argv)
{
	printf(" Calling dso_function()\n");
    	dso_function();

	return 0;
}

