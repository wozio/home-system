#!/usr/bin/env python

import logging
import service
import output

class dhwrec(service.service):
  def __init__(self):
    super(dhwrec, self).__init__("dhwrec22")

    self.output = output.output("relay-board", 1)

dhwrec_ = dhwrec()

def exit():
  global dhwrec_
  dhwrec_.exit()
  logging.debug("Services exit")