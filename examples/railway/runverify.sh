#!/bin/sh
LOGFILE=`date +%Y%m%d-%H%M`.log
cp /dev/null $LOGFILE

date >> $LOGFILE
for f in train-gate-fair      \
	 train-gate-mod       \
	 train-gate-fairtest  \
	 train-gate-test
do
    echo " ----- $f ----- " >> $LOGFILE
    verifyta -u -s -f $f $f.xml $f.q >> $LOGFILE
done
echo " ----- DONE ----- " >> $LOGFILE
date >> $LOGFILE

