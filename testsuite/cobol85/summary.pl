#!/usr/bin/perl -s
#
# Copyright (C) 2002 Keisuke Nishida
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

my $total_progs = 0;
my $total_tested = 0;
my $total_executed = 0;
my $total_skipped = 0;
my $total_error = 0;
my $total_crash = 0;

print ("Module  programs tested executed skipped error crash  details\n");
print ("------  -------- ------ -------- ------- ----- -----  -------\n");

while ($module = shift) {
  open(IN, "$module/report.txt") or die;
  while (<IN>) {
    if (/^Total *(\d+) *(\d+) *(\d+) *(\d+) *(\d+)/) {
      ($test, $pass, $fail, $delete, $inspect) = ($1, $2, $3, $4, $5);
    } elsif (/^Number of programs: *(\d+)/) {
      $progs = $1;
    } elsif (/^Successfully tested: *(\d+)/) {
      $tested = $1;
    } elsif (/^Successfully executed: *(\d+)/) {
      $executed = $1;
    } elsif (/^Test skipped: *(\d+)/) {
      $skipped = $1;
    } elsif (/^Compile error: *(\d+)/) {
      $error = $1;
    } elsif (/^Execute error: *(\d+)/) {
      $crash = $1;
    }
  }
  printf ("%-6s  %8d %6d %8d %6d %6d %5d  %d,%d,%d,%d/%d\n",
	  $module, $progs, $tested, $executed, $skipped, $error, $crash,
	  $pass, $fail, $delete, $inspect, $test);
  $total_progs += $progs;
  $total_tested += $tested;
  $total_executed += $executed;
  $total_skipped += $skipped;
  $total_error += $error;
  $total_crash += $crash;
}

print ("------  -------- ------ -------- ------- ----- -----  -------\n");
printf ("Total   %8d %6d %8d %6d %6d %5d\n",
	$total_progs, $total_tested, $total_executed, $total_skipped,
	$total_error, $total_crash);
