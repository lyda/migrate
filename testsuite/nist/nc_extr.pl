#!/usr/local/bin/perl -w
#
# Copyright (C) 2001 David Essex
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1,
# or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA 02111-1307 USA

###########################################################################
#                                                                         #
# Perl script to separate NIST test suite into separate files             #
#      and create a Makefile for the NIST test suite COBOL programs       #
#                                                                         #
###########################################################################


############### Globals ################

my $SW_start;
# Set to true to output program files
my $SW_output = "true";
#my $SW_output = "";
my $InString;
my @InStringArray;
my @InStringArray1;
my $FileNameOut;
my $FileName;
#my $FileNameLog = "logfile.txt";
#my $FileNameLog = $ARGV[1];
my $FileNameLog;
my @FileNameLogArray;
my $FileNameNIST = $ARGV[0];
my @SubPrmArray;
my $SubPrmItem;
my $PrgmOutCntr = 0;
my $SubDirDel = "/";
# Replace directory name if required
#my $SubDir0   = "t";
my $SubDir0   = ".";
my $SubDir1   = "copybooks";
my $SubDir    = "";
my $SubDirIdx = 0;
my @SubDirList  = ("copybooks", "XX", "CM", "DB", "IC", "IF", "IX", "NC", "OB", "RL", "RW", "SG", "SM", "SQ", "ST"); 
my @SubDirListCntr = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
#
my @PrgmList;
my @PrgmList_CM;
my @PrgmList_DB;
my @PrgmList_IC;
my @PrgmList_IF;
my @PrgmList_IX;
my @PrgmList_NC;
my @PrgmList_OB;
my @PrgmList_RL;
my @PrgmList_RW;
my @PrgmList_SG;
my @PrgmList_SM;
my @PrgmList_SQ;
my @PrgmList_ST;
#
my @ExtentionStringArray1 =  ("COBOL", "SUBRTN", "SUBPRG", "CLBRY", "DATA*");
# Replace values with preffered output extention
my @ExtentionStringArray2 =  ("CBL", "SUBRTN", "SUBPRG", "CLBRY", "DATA");
my @ExtentionStringArrayCntr =  (0, 0, 0, 0, 0);
#

#------------------ End globals ------------------# 

sub initialize_rtn ()
{
  if (! -d $SubDir0) {
     mkdir $SubDir0, 0755 ;
  }

}

sub create_subdir ()
{
  $SubDir = $SubDir0 . $SubDirDel . $SubDirList[$SubDirIdx];
  if (! -d $SubDir) {
     mkdir $SubDir, 0755 ;
  }
}

sub process_header ()
{

   print LOGFILE "* ---------------------------------------- *\n";
   print LOGFILE "head :" .  $_  . ":\n";
   $SW_start = 'True';
   $InString = $_;
   $InString =~ s/\*HEADER,//g;
#   $InString =~ s/ /:/;
   $InString =~ s/ //g;
   print LOGFILE "InString :" .  $InString  . ":\n";
   @InStringArray1 = split(/:/,  $InString);
   @InStringArray = split(/,/,  $InStringArray1[0]);
   for ($i=0; $i<=$#InStringArray; $i++) {
       print LOGFILE "InStringArray[$i] :" .  "$InStringArray[$i]"  . ":\n";
   }
}

sub create_makefile_list ()
{

# Does Perl have a case statement ?

#  push(@PrgmList, $FileName);
  
  if ($SubDirIdx == 2) {
      push(@PrgmList_CM, $FileName);
  }
  if ($SubDirIdx == 3) {
      push(@PrgmList_DB, $FileName);
  }
  if ($SubDirIdx == 4) {
      push(@PrgmList_IC, $FileName);
  }
  if ($SubDirIdx == 5) {
      push(@PrgmList_IF, $FileName);
  }
  if ($SubDirIdx == 6) {
      push(@PrgmList_IX, $FileName);
  }
  if ($SubDirIdx == 7) {
      push(@PrgmList_NC, $FileName);
  }
  if ($SubDirIdx == 8) {
      push(@PrgmList_OB, $FileName);
  }
  if ($SubDirIdx == 9) {
      push(@PrgmList_RL, $FileName);
  }
  if ($SubDirIdx == 10) {
      push(@PrgmList_RW, $FileName);
  }
  if ($SubDirIdx == 11) {
      push(@PrgmList_SG, $FileName);
  }
  if ($SubDirIdx == 12) {
      push(@PrgmList_SM, $FileName);
  }
  if ($SubDirIdx == 13) {
      push(@PrgmList_SQ, $FileName);
  }
  if ($SubDirIdx == 14) {
      push(@PrgmList_ST, $FileName);
  }
}

sub create_makefiles ()
{
 for ($SubDirIdx=1; $SubDirIdx<14; $SubDirIdx++) {
  if ($SubDirIdx == 2) {
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in CM array :" .  $#PrgmList_CM  . "\n";
     for ($i=0; $i<$#PrgmList_CM; $i++) {
          print LOGFILE "Element " . $i . " in CM array " .  $PrgmList_CM[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 3) {
      push(@PrgmList_DB, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in DB array :" .  $#PrgmList_DB  . "\n";
     for ($i=0; $i<$#PrgmList_DB; $i++) {
          print LOGFILE "Element " . $i . " in DB array " .  $PrgmList_DB[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 4) {
      push(@PrgmList_IC, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in IC array :" .  $#PrgmList_IC  . "\n";
     for ($i=0; $i<$#PrgmList_IC; $i++) {
          print LOGFILE "Element " . $i . " in IC array " .  $PrgmList_IC[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 5) {
      push(@PrgmList_IF, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in IF array :" .  $#PrgmList_IF  . "\n";
     for ($i=0; $i<$#PrgmList_IF; $i++) {
          print LOGFILE "Element " . $i . " in IF array " .  $PrgmList_IF[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 6) {
      push(@PrgmList_IX, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in IX array :" .  $#PrgmList_IX  . "\n";
     for ($i=0; $i<$#PrgmList_IX; $i++) {
          print LOGFILE "Element " . $i . " in IX array " .  $PrgmList_IX[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 7) {
      push(@PrgmList_NC, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in NC array :" .  $#PrgmList_NC  . "\n";
     for ($i=0; $i<$#PrgmList_NC; $i++) {
          print LOGFILE "Element " . $i . " in NC array " .  $PrgmList_NC[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 8) {
     push(@PrgmList_OB, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in OB array :" .  $#PrgmList_OB  . "\n";
     for ($i=0; $i<$#PrgmList_OB; $i++) {
          print LOGFILE "Element " . $i . " in OB array " .  $PrgmList_OB[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 9) {
      push(@PrgmList_RL, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in RL array :" .  $#PrgmList_RL  . "\n";
     for ($i=0; $i<$#PrgmList_RL; $i++) {
          print LOGFILE "Element " . $i . " in RL array " .  $PrgmList_RL[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 10) {
      push(@PrgmList_RW, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in RW array :" .  $#PrgmList_RW  . "\n";
     for ($i=0; $i<$#PrgmList_RW; $i++) {
          print LOGFILE "Element " . $i . " in RW array " .  $PrgmList_RW[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 11) {
      push(@PrgmList_SG, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in SG array :" .  $#PrgmList_SG  . "\n";
     for ($i=0; $i<$#PrgmList_SG; $i++) {
          print LOGFILE "Element " . $i . " in SG array " .  $PrgmList_SG[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 12) {
      push(@PrgmList_SM, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in SM array :" .  $#PrgmList_SM . "\n";
     for ($i=0; $i<$#PrgmList_SM; $i++) {
          print LOGFILE "Element " . $i . " in SM array " .  $PrgmList_SM[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 13) {
      push(@PrgmList_SQ, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in SQ array :" .  $#PrgmList_SQ  . "\n";
     for ($i=0; $i<$#PrgmList_SQ; $i++) {
          print LOGFILE "Element " . $i . " in SQ array " .  $PrgmList_SQ[$i]  . "\n";
     }
  }
  if ($SubDirIdx == 14) {
      push(@PrgmList_ST, $FileName);
     print LOGFILE "* ---------------------------------------- *\n";
     print LOGFILE "Number of elements in ST array :" .  $#PrgmList_ST  . "\n";
     for ($i=0; $i<$#PrgmList_ST; $i++) {
          print LOGFILE "Element " . $i . " in ST array " .  $PrgmList_ST[$i]  . "\n";
     }
  }
 }
}

sub determine_FileNameOut ()
{
   $i = 0;
   $ftype = "";
   $ftlist = $InStringArray[$#InStringArray];
   $ftdir = substr $ftlist, 0, 2;
#   print LOGFILE "Debug : \$ftlist=" . $ftlist . 
#                 ", \$ftdir="  .$ftdir . 
#                 "\;\n";

#   $j = $#InStringArray - 1;
#  Determine file extention and substitution
   $fext = $InStringArray[$#InStringArray];
   for ($i=0; $i<=$#ExtentionStringArray1; $i++) {
#     print LOGFILE "Debug : \$ExtentionStringArray1[" . 
#                   $i . 
#                   "]=" . 
#                   $ExtentionStringArray1[$i] .
#                   ", " .
#                   " \$InStringArray[" . $j . "]=" .
#                   $InStringArray[$#InStringArray - 1] .
#                   ":\n";
     if ($ExtentionStringArray1[$i] eq $InStringArray[$#InStringArray - 1]) {
       $fext = $ExtentionStringArray2[$i];
       $ftype = $i;
       $ExtentionStringArrayCntr[$i]++;
       $i= $#ExtentionStringArray1;
     }
   }

#  Determine program sub directory
   if ($ftype eq 3) {
      $SubDir = $SubDirList[0];
      $SubDirListCntr[0]++;
      $SubDirIdx = 0;
   }
   else {
      $SubDir = $SubDirList[1];
      $SubDirListCntr[1]++;
      for ($i=2; $i<=$#SubDirList; $i++) {
          if ($SubDirList[$i] eq $ftdir) {
             $SubDir = $SubDirList[$i];
             $SubDirIdx = $i;
             $SubDirListCntr[1]--;
             $SubDirListCntr[$i]++;
             $i = $#SubDirList;
          }
      }
   }

   $FileName    = $InStringArray[$#InStringArray] . "." . $fext;
   
   $FileNameOut = $SubDir0 . 
                  $SubDirDel . 
                  $SubDir . 
                  $SubDirDel .
                  $FileName ;
   
   print LOGFILE "FileNameOut :" . "$FileNameOut" . ":\n";

   if ($SubDirListCntr[$SubDirIdx] == 1) {
      create_subdir;
   }
   if ($SubDirIdx > 1) {
      create_makefile_list;
   }

}

sub makefile_list1 ()
{

print LOGFILE "Number of elements in array :" .  $#SubPrmArray  . "\n";
print LOGFILE "* ---------------------------------------- *\n";

#sort @SubPrmArray;

@InStringArray1 = split(/:/,  $SubPrmArray[0]);
$element1 = $InStringArray1[0];
$element2 = $InStringArray1[0];
foreach $element (@SubPrmArray) {
   @InStringArray1 = split(/:/,  $element);
   if ($element1 eq $InStringArray1[0]) {
      $element2 = $element2 . " " . $InStringArray1[1];
   }
   else {
      print LOGFILE "$element2" . "\n";
      $element1 = $InStringArray1[0];
      $element2 = $InStringArray1[0] . " " . $InStringArray1[1];
   }
}
}

if ($#ARGV != 0) {
   print "ERROR: invalid number of arguments " . ($#ARGV + 1) . " != 1\n";
   die;
}

@FileNameLogArray = <logfile*txt>;
$FileNameLog = "logfile." . ($#FileNameLogArray + 2) . ".txt";
if (-f $FileNameLog) {
   print "ERROR: file FileNameLog \"$FileNameLog\" exists\n";
   die;
}
#print "FileNameLog = $FileNameLog;\n";

#die;

#open (NISTFILE, "< $ARGV[0]")
open (NISTFILE, "< $FileNameNIST")
     or die "unable to open file \"$FileNameNIST\"";
open (LOGFILE, "> $FileNameLog ")
     or die "unable to run \"$FileNameLog\"";

# Remove comment to create directories where for output program files
initialize_rtn;

while (<NISTFILE>)
{
 chop;
 if ($SW_start) {
    if (/\*END-OF,/) {
        print LOGFILE "tail :" . $_ . ":\n";
        $SW_start = '';
        $PrgmOutCntr++;
        if ($SW_output) {
           close(PGMFILE);
        }
    }
    else {
        if ($SW_output) {
           print PGMFILE $_ . "\n";
        }
    }
 }
 else {
    if (/\*HEADER,/) {
        process_header;
        determine_FileNameOut;
        if ($SW_output) {
            open (PGMFILE, "> $FileNameOut")
                  or die "unable to open output file \"$FileNameOut\"";
        }
#        if (($#InStringArray == 3) and ("$InStringArray[2]" eq "SUBPRG")) {
#         if ($#InStringArray == 3) {
#            $SubPrmItem = $InStringArray[1] . "." . $InStringArray[0] . ":" .
#                          $InStringArray[3] . "." . $InStringArray[2];
#     	   print LOGFILE "SubPrmItem:" . "$SubPrmItem" . ":\n";
#            push(@SubPrmArray, $SubPrmItem);
#         }
    }
    else {
        print LOGFILE "* ---------------------------------------- *\n";
    	print LOGFILE "ignoring :" .  $_  . ":\n";
    }
 }
}

print LOGFILE "* ---------------------------------------- *\n";
print LOGFILE "Number of output programs :" .  $PrgmOutCntr  . "\n";
print LOGFILE "****\n";
for ($y=0; $y<=$#SubDirList; $y++) {
    print LOGFILE "Number of files in directory " .
                  $SubDirList[$y] .
                  " is " . 
                  $SubDirListCntr[$y] .
                  "\n";
}
print LOGFILE "****\n";
for ($y=0; $y<=$#ExtentionStringArray1; $y++) {
    print LOGFILE "Number of files by extention " .
                  $ExtentionStringArray2[$y] .
                  " is " . 
                  $ExtentionStringArrayCntr[$y] .
                  "\n";
}
print LOGFILE "****\n";

create_makefiles;

close(LOGFILE);

__END__
