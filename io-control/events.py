#!/usr/bin/env python

import configuration
import wtimer

events = {}

def init():
  logging.debug("Events init")
  global events
  for e in configuration.events:
    if e["type"] == "wtimer":
        events[e["name"]] = wtimer.wtimer(e["data"])

def exit():
  global outputs