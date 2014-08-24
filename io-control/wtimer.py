#!/usr/bin/env python

import logging
import threading
import sortedcontainers
import datetime

class wtimer():
  def __init__(self):

    self.last_state = None
    self.callback = None
    self.timeouts = sortedcontainers.SortedDict()

    self.timer = threading.Timer(1, self.on_timeout)
    self.timer.start()

  def exit(self):
    self.timer.cancel()
    
  def set_callback(self, callback):
    self.callback = callback
  
  def on_timeout(self):
    # restart the timer
    self.timer = threading.Timer(1, self.on_timeout)
    self.timer.start()
    
    if (self.callback == None):
      return

    if len(self.timeouts) == 0:
      return

    now = datetime.datetime.now()
    nowsec = now.weekday() * 86400 + now.hour * 3600 + now.minute * 60 + now.second

    ind = self.timeouts.bisect(nowsec)
    tlen = len(self.timeouts)
    # if provide time is bigger than last in timeouts () or
    # lower than first one
    if ind == tlen or ind == 0:
      ind = tlen - 1
    else:
      ind -= 1
      
    new_state = self.timeouts.get(self.timeouts.iloc[ind])
    if new_state != self.last_state:
      self.last_state = new_state
      self.callback(new_state)
    
  def add(self, day_of_week, time_of_day, state):
    l = time_of_day.split(':')
    todsec = int(l[0]) * 3600 + int(l[1]) * 60
    if len(l) > 2:
      todsec += int(l[2])
    timeout = day_of_week*86400 + todsec
    self.timeouts[timeout] = state
    