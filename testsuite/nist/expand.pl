#!/usr/bin/perl
# 
# Copyright (C) 2001-2002 Keisuke Nishida
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
# Boston, MA 02111-1307 USA

open (IN, shift) or die;

while (<IN>) {
  chop;
  if (/^      \*HEADER,([^,]*),([^, ]*)(,([^,]*),([^, ]*))?/) {
    my ($type, $prog, $subt, $subr) = ($1, $2, $4, $5);
    my $module = substr($prog, 0, 2);
    my $name = '';
    if ($subt) {
      if ($subt eq "SUBPRG") {
	$name = "$subr.SUB";
      } elsif ($subt eq "SUBRTN") {
	$name = "lib/$subr.CBL";
      }
      mkdir "$module/lib" unless (-e "$module/lib");
    } elsif ($type eq "COBOL") {
      $name = "$prog.CBL";
    } elsif ($type eq "DATA*") {
      $name = "$prog.DAT";
    } elsif ($type eq "CLBRY") {
      $module = "copy";
      $name = "$prog";
    }
    if ($name) {
      mkdir $module unless (-e $module);
      open (OUT, "> $module/$name") or die;
      while (<IN>) {
	last if /^      \*END/;
	print OUT;
      }
    } else {
      while (<IN>) {
	last if /^      \*END/;
      }
    }
  }
}
