#!/bin/sh

# Definition of PAKCS installation to be used:
PAKCSBIN=/home/mh/pakcs/bin

ORIGDIR=`pwd`                          # original directory (builtin)
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
PROGNAME=`basename $PROGNAME`          # base name of program
ABSPROGDIR="`cd \"$PROGDIR\" 2>/dev/null && pwd || echo \"$PROGDIR\"`"  # get absolute path

CURRYPATH="$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config"
export CURRYPATH

$PAKCSBIN/pakcs -r Main
