#!/usr/bin/env python

import logging
import iomap

class Display:

    def __init__(self, name, data):
        logging.info("Created display '%s'", name)

        self.name = name
        self.type = data["type"]
        self.fromio = data["from"] 

    def get(self):
        try:
            return iomap.Iomap[self.fromio].get()
        except KeyError:
            logging.error("Attempt to read from not known io (%s %s)",
                self.name, self.fromio)
        except RuntimeError:
            logging.error("Attempt to read from not ready io (%s %s)",
                self.name, self.fromio)
        return None
    