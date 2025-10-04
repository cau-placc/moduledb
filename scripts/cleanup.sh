#!/bin/sh
# Remove older temporary files in the module DB

# start the corresponding web script:
curl --max-time 30 -s https://moduldb.informatik.uni-kiel.de/cleanup.cgi > /dev/null 2>&1

# insert in crontab with: crontab -e --> insert line:
# 5,35 * * * * .../scripts/cleanup.sh
# Effect: cleanup is called every 30 minutes
