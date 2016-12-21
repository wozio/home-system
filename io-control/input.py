#!/usr/bin/env python

import time
import logging
import discovery

state_unknown = 0
state_ok = 1

class input(object):

    def __init__(self, name, service, id):
        self.id = id
        self.service = service
        self.name = name
        self.state = state_unknown
        self.value = 0
        self.callbacks = []
        
        discovery.register(self.on_service)

        self.history = []
        # first history entry, unknown state, 0 value ignored anyways due to state
        self.history.append((time.time(), state_unknown, 0))

        logging.info("Created IO '%s' with service=%s and id=%d", name, service, id)

    def exit(self):
        pass

    def on_service(self, new_service, available):
        if not available:
            if new_service == self.service:
                logging.debug("IO service '%s' is not available for IO '%s'", new_service, self.name)
                self.on_state_change(state_unknown, 0)

    def get(self):
        return self.state, self.value

    def on_state_change(self, state, value):
        
        if state != self.state:
            logging.debug("'%s' state changed %d->%d", self.name, self.state, state)
            self.state = state

        if self.state == state_ok and value != self.value:
            logging.debug("'%s' value changed %s->%s", self.name, str(self.value), str(value))
            self.value = value

        self.history.append((time.time(), state, value))
        for c in self.callbacks:
            c()

    def subscribe(self, callback):
        self.callbacks.append(callback)
        callback()

    def get_history(self):
        return self.history
