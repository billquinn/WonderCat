# Define cron job command.
wikiDataCron = "*/10 * * * * /usr/bin/python3 /root/shiny-server/apps/sample-apps/00_wondercat/fetchWikiData.py"

# Check if the cron job already exists to avoid duplication
(crontab -l | grep -v -F "$wikiDataCron"; echo "$wikiDataCron") | crontab -
echo "✅ Scheduled cron job to run every 5 minutes."

# Schedule a one-time cron job to remove it at midnight
(crontab -l; echo "59 23 * * * crontab -r") | crontab -
echo "✅ Scheduled removal of cron job at midnight."