# COBOL compiler configuration					-*- sh -*-

# Value: int
tab-width: 8
text-column: 72

# Value: `auto', `free', `fixed'
source-format: fixed

# Value: `native', `big-endian'
binary-ordering: big-endian

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
invalid-value: warning
redefines-occurs-item: warning
