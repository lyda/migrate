#include	<stdio.h>
#include	<libcob.h>

int
main (int argc, char **argv)
{
	int (*func)();

	if ( argc <= 1 ) {
		fprintf(stderr, "Number of parameters incorrect\n");
		return 1;
	}
	cob_init (argc - 1, &argv[1]);
	func = cob_resolve (argv[1]);
	if (func == NULL) {
		cob_call_error ();
	}
	return func();
}
