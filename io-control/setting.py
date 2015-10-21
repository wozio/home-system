#!/usr/bin/env python

import logging
import rules

class Setting:

    def __init__(self, name, data):
        logging.info("Created setting '%s'", name)

        self.name = name
        self.values = data["values"]
        self.value = self.values.iterkeys().next()
        self.set(data["default"])

    def set(self, value):
        if value not in self.values:
            logging.warn("Required value '%s' no in values list", value)
            return;

        logging.debug("Setting '%s' set to value '%s'", self.name, value)
        # disable previous rule
        rules.rules[self.values[self.value]["rule"]].disable()
        # enable new rule
        rules.rules[self.values[value]["rule"]].enable()

        self.value = value

    def get(self):
        return self.value;

