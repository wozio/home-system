#!/usr/bin/env python

import socket
import struct
import logging

class input:

    def __init__(self, input_name, input_service, input_id):
        self.id = input_id
        self.service = input_service
        self.name = input_name
        self.state = 0
        self.callbacks = []
        self.ready = False

        logging.info("Created input '%s' with service=%s and id=%d", input_name, input_service, input_id)

    def exit(self):
        pass

    def get(self):
        if not self.ready:
            raise RuntimeError("Input not ready")
        return self.state

    def on_state_change(self, state):
        if not self.ready or state != self.state:
            self.ready = True

            logging.debug("'%s' state changed %f->%f", self.name, self.state, state)
            self.state = state
            for c in self.callbacks:
                c()

    def subscribe(self, callback):
        self.callbacks.append(callback)
        callback()
