#!/usr/bin/env python

import logging
import setting
import display

class ioservice:

    def __init__(self, name, displays_, settings_):
        self.name = name
        self.settings = {}
        self.displays = {}

        logging.info("Created ioservice '%s'", name)

        for s in settings_:
            self.settings[s["name"]] = setting.Setting(s["name"], s["data"])
            
        for d in displays_:
            self.displays[d["name"]] = display.Display(d["name"], d["data"])
