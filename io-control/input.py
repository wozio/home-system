#!/usr/bin/env python

import logging

state_unknown = 0
state_ok = 1

class input:

    def __init__(self, input_name, input_service, input_id):
        self.id = input_id
        self.service = input_service
        self.name = input_name
        self.state = state_unknown
        self.value = None
        self.callbacks = []

        logging.info("Created input '%s' with service=%s and id=%d", input_name, input_service, input_id)

    def exit(self):
        pass

    def get(self):
        if self.state != state_ok:
            raise RuntimeError("Input not OK")
        return self.value

    def on_state_change(self, state, value):
        if state != self.state:
            logging.debug("'%s' state changed %d->%d", self.name, self.state, state)
            self.state = state
            if self.state != state_ok:
                self.value = None
        if self.state == state_ok and value != self.value:
            logging.debug("'%s' value changed %s->%s", self.name, str(self.value), str(value))
            self.value = value
            for c in self.callbacks:
                c()

    def subscribe(self, callback):
        self.callbacks.append(callback)
        callback()
