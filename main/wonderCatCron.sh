#!/bin/sh

# Define cron job command.
wonderCRON_JOB="*/10 * * * * /usr/bin/python3 /root/shiny-server/apps/sample-apps/00_wondercat/fetchWonderCat.py >> /root/shiny-server/apps/sample-apps/00_wondercat/execution_logs/cron_log.txt 2>&1"
# Check if the cron job already exists to avoid duplication
(crontab -l | grep -v -F "$wonderCRON_JOB"; echo "$wonderCRON_JOB") | crontab -
echo "✅ Scheduled cron job to run every 10 minutes."
# # Schedule a one-time cron job to remove it at midnight
# (crontab -l; echo "59 23 * * * crontab -r") | crontab -
# echo "✅ Scheduled removal of cron job at midnight."