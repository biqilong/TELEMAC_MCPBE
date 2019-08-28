#!/bin/bash
#
# Author: Y. Audouin
# Date: 16/07/2014
#
#Returns incorrect lines
if [[ $# -lt 1 ]]; then
  echo "incorrect number of argument"
  echo "usage: check_code.sh path_to_code"
  exit 1
fi
if [[ $1 -eq "-h" ]]; then
  echo "Script checking some points of the coding convention "
  echo "for all the .f and .F in the folder given in parameter"
  echo "It will generate 5 files:"
  echo "-- indent.log : contains the line where the indentation is not a 6 + x*2"
  echo "-- comments.log : checks that the character used for comments is '!'"
  echo "-- continuation.log : checks that the character for continuation is '&'"
  echo "-- lowercase.log : checks that there are no lowercase code"
  echo "-- linetoolong.log : checks that there are no line wider than 72 character (expect comments)"
  echo "-- invalidchar.log : checks that there are no tabs or CLRF (Windows \\n)"
  exit 1
fi
#
# Indent errors
#
INDENT=cc_1-indent.log
grep -ER -n $2 '^(\ ){9}[^\ ]|^(\ ){7}[^\ ]|^(\ ){15}[^\ ]' $1 --include=*.[fF] > $INDENT
#
# Comments error
#
COMMENT=cc_2-comment.log
grep -ER -n $2 '^[^!\n0-9#\ ]' $1 --include=*.[fF] > $COMMENT
#
# Continuation line error
#
CONTINUATION=cc_3-continuation.log
grep -ER -n $2 '^(\ ){5}[^\&\ ]' $1 --include=*.[fF] > $CONTINUATION
#
# Lowercase error
#
LOWERCASE=cc_4-lower-case.log
grep -ER -n $2 '^[^!#\"'\'']*[azertyuiopqsdfghjklmnbvcxw]' $1 --include=*.[fF] > $LOWERCASE
#
# Line too long error
#
LINETOOLONG=cc_5-line-too-long.log
grep -ER -n $2 '^[^!]{73}' $1 --include=*.[fF] > $LINETOOLONG
#
# Invalid character error
#
INVALIDCHAR=cc_0-invalid-char.log
grep -PR -n $2 '\t|\r' $1 --include=*.[fF] > $INVALIDCHAR

# Coding convention not applied to mascaret and nestor for now
sed -i -e "/mascaret\//d" *.log
sed -i -e "/nestor\//d" *.log

echo '*****************'
echo '     errors      '
echo '*****************'
wc -l $INVALIDCHAR
wc -l $INDENT
wc -l $COMMENT
wc -l $CONTINUATION
wc -l $LOWERCASE
wc -l $LINETOOLONG
