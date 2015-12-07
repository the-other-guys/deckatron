#!/usr/bin/env python

# */30 * * * * /usr/bin/env python /home/deckatron/cleanup.py >/home/deckatron/cleanup.log 2>&1

import os, os.path, time

path = "/home/deckatron/deckatron/.deckatron/decks"

def file(n):
  return os.path.join(path, n)

def get_time(f):
  stat = os.stat(file(f))
  return time.ctime(stat.st_atime) #, time.ctime(stat.st_mtime)]

decks = [f for f in os.listdir(path) if os.path.isfile(file(f))]
decks.remove("deck-welcome.edn")
decks.remove("deck-cheatsheet.edn")
decks.sort(key=get_time)

print "Cleaning up..."
for f in decks[:-10]:
  print "  Moving", f
  os.rename(file(f), file("../removed/" + f))
