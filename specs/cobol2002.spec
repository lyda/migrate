# COBOL compiler specification					-*- sh -*-

name: "COBOL 2002"

# Value: `yes', `no'
binary-bigendian: no
redefines-occurs: no

# Value:         digits  bytes
#                ------  -----
# `1-2-4-8'      1 -  2      1
#                3 -  4      2
#                5 -  9      4
#               10 - 18      8
# 
# `2-4-8'        1 -  4      2
#                5 -  9      4
#               10 - 18      8
binary-size: 1-2-4-8

# Value: `warning', `error'

invalid-value:		error

# Value: `ok', `archaic', `obsolete', `unconformable'

author-paragraph:		unconformable
memory-size-clause:		unconformable
multiple-file-tape-clause:	unconformable
label-records-clause:		unconformable
value-of-clause:		unconformable
data-records-clause:		unconformable
alter-statement:		unconformable
goto-statement-without-name:	unconformable
stop-literal-statement:		unconformable
debugging-line:			obsolete
padding-character-clause:	obsolete
next-sentence-phrase:		archaic
