#!/bin/sh
# save all DB data in term files:

# log file for logging save actions:
LOGFILE=$HOME/tmp/MDB/SAVE.LOG
mkdir -p `dirname $LOGFILE`
touch $LOGFILE
chmod 644 $LOGFILE

exit_with_message() {
  echo "ERROR OCCURRED WHILE EXECUTING $0 !!!!!!!!!!!!!!!!!"
  echo "Contents of $LOGFILE:"
  cat $LOGFILE
  exit
}

trap "exit_with_message" 1 2 13 15

curl --max-time 30 -s https://moduldb.informatik.uni-kiel.de/save.cgi >> $LOGFILE 2>&1
if [ $? -gt 0 ] ; then exit_with_message ; fi

echo "Module DB saved at `date`" >> $LOGFILE
echo "==================================================" >> $LOGFILE

# insert in crontab with: crontab -e --> insert line:
# 10 6 * * * .../scripts/save.sh
# Effect: save is called every day at 6:10
