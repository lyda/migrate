# COBOL compiler configuration					-*- sh -*-

# Value: int
tab-width: 8
text-column: 72

# Value: `auto', `free', `fixed'
source-format: fixed

# Value: `yes', `no'
device-predefine: no

# Value: `yes', `no'
filename-mapping: no

# Value:             sign  0123456789
#                    ----  ----------
# `ascii'        positive  0123456789
#                negative  pqrstuvwxy
#
# `ebcdic'       positive  {ABCDEFGHI
#                negative  }JKLMNOPQR
#
# `ascii10'      positive  0123456789
#                negative  @ABCDEFGHI
#
# `ascii20'      positive  @ABCDEFGHI
#                negative  PQRSTUVWXY
display-sign: ascii

# Value:         signed  unsigned  bytes
#                ------  --------  -----
# `2-4-8'        1 -  4                2
#                5 -  9                4
#               10 - 18                8
#
# `1-2-4-8'      1 -  2                1
#                3 -  4                2
#                5 -  9                4
#               10 - 18                8
#
# `1--8'         1 -  2    1 -  2      1
#                3 -  4    3 -  4      2
#                5 -  6    5 -  7      3
#                7 -  9    8 -  9      4
#               10 - 11   10 - 12      5
#               12 - 14   13 - 14      6
#               15 - 16   15 - 16      7
#               17 - 18   17 - 18      8
binary-size: 1-2-4-8

# Value: `native', `big-endian'
binary-byteorder: big-endian

# Value: `yes', `no'
binary-full-print: no
