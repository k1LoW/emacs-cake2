#!/bin/sh
EMACS=emacs
OUTPUT=/tmp/.el-expectations
$EMACS -q --no-site-file --batch  -L . -L t/extlib/  -l el-expectations -l cake2 -f batch-expectations $OUTPUT ./cake2.el
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
