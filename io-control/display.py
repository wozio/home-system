#!/usr/bin/env python

import logging
import iomap
import yami

class Display:

    def __init__(self, name, data):
        logging.info("Created display '%s'", name)

        self.name = name
        self.type = data["type"]
        self.fromio = data["from"]
        self.change_callback = None
        iomap.Iomap[self.fromio].subscribe(self.on_change)

    def getio(self):
        return iomap.Iomap[self.fromio]

    def get(self):
        try:
            return getio().get()
        except KeyError:
            logging.error("Attempt to read from not known io (%s %s)",
                self.name, self.fromio)
            return output.state_unknown, 0
      
    def subscribe(self, callback):
        self.change_callback = callback
    
    def on_change(self):
        if self.change_callback != None:
            self.change_callback(self)
    
    def prepare(self):
        params = yami.Parameters()
        params["name"] = self.name
        params["type"] = self.type
        params["state"], params["value"] = self.get()

        return params

    def prepare_history(self):
        params = yami.Parameters()
        params["name"] = self.name
        try:
          params["history"] = self.getio().get_history()
        except KeyError:
            logging.error("Attempt to read from not known io (%s %s)",
                self.name, self.fromio)

        return params
