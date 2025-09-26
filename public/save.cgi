#!/bin/sh

# directory with all data:
DATADIR=../mdbData
# directory to save all data:
SAVEDATADIR=../SAVE

echo "Content-type: text/html"
echo
echo "<html>"
echo "<body>"
echo "<pre>"
echo "Save all DB data in term files..."
wget -O - https://moduldb.informatik.uni-kiel.de/show.cgi?saveDB
echo
echo "Store term files in a tar file..."
cd $DATADIR && tar -cvzf $SAVEDATADIR/save_`date +%Y%m%d_%H%M%S`.tgz *.terms
echo "...done!"
echo "</pre>"
echo "</body>"
echo "</html>"
