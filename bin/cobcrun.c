/*
 * Copyright (C) 2004-2005 Roger While
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License 
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

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
	cob_stop_run ( func() );
}
