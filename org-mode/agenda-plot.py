import time
t1 = time.time()
import json
import numpy as np
import re
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import requests
import datetime
import os, sys

AUTH_TOKEN=os.getenv("AUTH_TOKEN")
LIVE_AGENDA_FILE=os.getenv("LIVE_AGENDA_FILE")
GOAL_NAME=os.getenv("GOAL_NAME")
BACKUPS_DIR=os.getenv("BACKUPS_DIR")
START_DATE=os.getenv("START_DATE")

def date_2_agenda_size(date):
    if date == datetime.date.today():
        filename = LIVE_AGENDA_FILE
    else:
        filename = date.strftime(f"{BACKUPS_DIR}/agenda.%Y-%m-%d.org")
    with open(filename) as f:
        contents = f.read()
    right_now = re.search("Things to do right now: (\d+)", contents)[1]
    backlog = re.search("Items in the backlog: (\d+)", contents)[1]
    return int(right_now) + int(backlog)

freshenings = {}

t2 = time.time()
resp = requests.get(
    f"https://www.beeminder.com/api/v1/users/gfredericks/goals/{GOAL_NAME}/datapoints.json",
    params={"auth_token": AUTH_TOKEN}
)
resp.raise_for_status()
print(f"Datapoints request took {time.time() - t2:.1f}s")

all_data = json.loads(resp.content)
for item in all_data:
    the_date = datetime.datetime.strptime(item["daystamp"], "%Y%m%d").date()
    val = item["value"]
    if the_date not in freshenings:
        freshenings[the_date] = val
    else:
        freshenings[the_date] += val


start_date = datetime.datetime.strptime(START_DATE, "%Y-%m-%d").date()
end_date = datetime.date.today()
dates = []
agenda_size = []
freshening = []

while start_date <= end_date:
    tot = date_2_agenda_size(start_date)
    dates.append(start_date)
    agenda_size.append(tot)
    if start_date in freshenings:
        freshening.append(freshenings[start_date])
    else:
        freshening.append(0)
    start_date = start_date + datetime.timedelta(days=1)

fig, ax = plt.subplots()

ax2 = ax.twinx()

ax.plot(dates, agenda_size)
ax2.bar(dates, freshening, align='center', color='blue')
ax2.set_ylim([0, 10])
ax.set_ylim([0, None])

ax.set_ylabel('Agenda items')
ax2.set_ylabel('Freshenings')

locator = mdates.MonthLocator()
formatter = mdates.DateFormatter('%Y-%m-%d')
ax.xaxis.set_major_locator(locator)
ax.xaxis.set_major_formatter(formatter)

plt.savefig(sys.argv[1], format="png")

print(f"Finished in {time.time() - t1:.1f}s at {datetime.datetime.now()}")
print("OK!")
