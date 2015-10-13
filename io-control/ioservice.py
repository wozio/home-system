#!/usr/bin/env python

import logging
import setting

class ioservice:

  def __init__(self, name, settings_):
    self.name = name
    self.settings = []

    logging.info("Created ioservice '%s'", name)
    
    for s in settings_:
        self.settings.append = setting.Setting(s["name"], s["data"])

    
