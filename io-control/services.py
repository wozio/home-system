#!/usr/bin/env python

import logging
import threading
import service
import output

class dhwrec(service.service):
  def __init__(self):
    super(dhwrec, self).__init__("dhwrec22")

    self.output = output.output("relay-board", 1)
    
    self.timer2 = threading.Timer(20, self.on_timeout2)
    self.timer2.start()

  def exit(self):
    self.timer2.cancel()
    super(dhwrec, self).exit()
  
  def on_timeout2(self):
    state = self.output.get_state()
    if state != -1:
      if state == 0:
        state = 1
      else:
        state = 0
      self.output.set_state(state)
    
      self.timer2 = threading.Timer(20, self.on_timeout2)
      self.timer2.start()

def init():
  global dhwrec_
  dhwrec_ = dhwrec()

def exit():
  global dhwrec_
  dhwrec_.exit()
  logging.debug("Services exit")
