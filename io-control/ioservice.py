#!/usr/bin/env python

import logging

class ioservice:

  def __init__(self, name, settings):
    self.name = name

    logging.info("Created ioservice '%s'", name)

    
