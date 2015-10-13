#!/usr/bin/env python

import logging
import rules

class Setting:

    def __init__(self, name, data):
        logging.info("Created setting '%s'", name)

        self.value = -1
        self.name = name
        self.values = data["values"]
        self.set_value(data["default"])

    def set_value(self, value):
        # disable previous rule
        if self.value != -1:
            rules.rules[self.values[self.value]["rule"]].disable()
        # enable new rule
        if value != -1 and value < len(self.values):
            rules.rules[self.values[value]["rule"]].enable()

        if value < len(self.values):
            self.value = value
        else:
            self.value = -1;
