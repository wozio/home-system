#!/usr/bin/env python

import logging
import service
import output
import wtimer

class dhwrec(service.service):
  def __init__(self):
    super(dhwrec, self).__init__("dhwrec22")

    self.output = output.output("relay-board", 1)
    
    self.wtimer = wtimer.wtimer()
    
    for i in range(7):
      self.wtimer.add(i, "06:00", 1)
      self.wtimer.add(i, "23:00", 0)
      
    for i in range(6):
      self.wtimer.add(i, "07:15", 0)
      self.wtimer.add(i, "16:00", 1)
      
    self.wtimer.set_callback(self.on_timeout2)

  def exit(self):
    self.wtimer.exit()
    super(dhwrec, self).exit()
  
  def on_timeout2(self, state):
    logging.debug("Timer trigger to state %d", state)
    self.output.set_state(state)

def init():
  global dhwrec_
  dhwrec_ = dhwrec()

def exit():
  global dhwrec_
  dhwrec_.exit()
  logging.debug("Services exit")
