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
    self.wtimer.add(4, "23:16", 1)
    self.wtimer.add(4, "23:17", 0)
    
    self.wtimer.set_callback(self.on_timeout2)

  def exit(self):
    self.wtimer.exit()
    super(dhwrec, self).exit()
  
  def on_timeout2(self, state):
    self.output.set_state(state)

def init():
  global dhwrec_
  dhwrec_ = dhwrec()

def exit():
  global dhwrec_
  dhwrec_.exit()
  logging.debug("Services exit")
