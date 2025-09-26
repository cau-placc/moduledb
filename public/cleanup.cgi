#!/bin/sh
# Remove older temporary files

echo "Content-type: text/html"
echo
echo "<html>"
echo "<body>"
echo "<pre>"
cd `dirname $0`
find . -name tmp\*  -amin +30 -exec /bin/rm \{\} \;
echo "</pre>"
echo "</body>"
echo "</html>"

# insert in crontab with: crontab -e --> insert line:
# 5,35 * * * * $HOME/public_html/studiengaenge/cleanup
# Effect: cleanup is called every 30 minutes
