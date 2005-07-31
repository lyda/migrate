#include <stdio.h>
#include <string.h>

int
main ()
{
#ifdef __i386__
#if ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 ) || __GNUC__ > 3
	char vendor_string[16];
	int eax, ebx, edx, ecx;
	int i, hv;
	int family, model, stepping;

	__asm__ (".byte 0x0f,0xa2"
	: "=a" (hv), "=b" (ebx), "=d" (edx), "=c" (ecx) : "0" (0));

	*(int *) (vendor_string + 0) = ebx;
	*(int *) (vendor_string + 4) = edx;
	*(int *) (vendor_string + 8) = ecx;
	vendor_string[12] = 0;
	i = 1;

	__asm__ (".byte 0x0f,0xa2"
	: "=a" (eax), "=b" (ebx), "=d" (edx), "=c" (ecx) : "0" (i));

	family = (eax >> 8) & 0xf;
	model = (eax >> 4) & 0xf;
	stepping = eax & 0xf;
	if (family == 0xf) {
		/* "extended" mode. */
		family += (eax >> 20) & 0xff;
		model += (eax >> 12) & 0xf0;
	}

	if (strcmp (vendor_string, "GenuineIntel") == 0) {
		if (family == 5) {
			printf ("-march=pentium");
		} else if (family == 6) {
			if (model <= 2) {
				printf ("-march=pentiumpro");
			} else if (model >= 3 && model <= 6) {
				printf ("-march=pentium2");
			} else {
				printf ("-march=pentium3");
			}
		}
		else if (family == 15) {
			printf ("-march=pentium4");
		}
	} else if (strcmp (vendor_string, "AuthenticAMD") == 0) {
		if (family == 6) {
			printf ("-march=athlon");
		}
	}
#endif
#endif
	return 0;
}
