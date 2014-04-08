#!/bin/sh
if [ -z "$EMACS" ] ; then
    EMACS=emacs
fi
OUTPUT=/tmp/.el-expectations
cask exec $EMACS -Q --batch  -L . -L t/extlib/ -l el-expectations -l cake2 -f batch-expectations $OUTPUT ./cake2.el
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
