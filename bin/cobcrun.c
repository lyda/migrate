/*
 * Copyright (C) 2004-2007 Roger While
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
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include	<stdio.h>
#include	<string.h>
#include	<libcob.h>

int
main (int argc, char **argv)
{
	union {
		int	(*func)();
		void	*func_void;
	} unifunc;

	if ( argc <= 1 ) {
		fprintf(stderr, "Usage: cobcrun PROGRAM [param ...]\n");
		return 1;
	}
	if (strlen (argv[1]) > 32) {
		fprintf(stderr, "Invalid PROGRAM name\n");
		return 1;
	}
	cob_init (argc - 1, &argv[1]);
	unifunc.func_void = cob_resolve (argv[1]);
	if (unifunc.func_void == NULL) {
		cob_call_error ();
	}
	cob_stop_run ( unifunc.func() );
}
