#!/usr/bin/env python

import socket
import struct
import logging
import yami
import yagent
import discovery

class output:

    def __init__(self, output_name, output_service, output_id):
        self.id = output_id
        self.service = output_service
        self.name = output_name
        self.state = 0
        self.callbacks = []
        self.ready = False
        self.wanted_state = 0

        logging.info("Created output '%s' with service=%s and id=%d", self.name, output_service, output_id)

    def get(self):
        if not self.ready:
            raise RuntimeError("Output not ready")
        return self.state

    def set(self, state):
        if state != self.wanted_state:
            self.wanted_state = state
            logging.debug("'%s' state set to %f", self.name, state)

        if self.wanted_state != self.state:
            self.set_state()

    def set_state(self):
        params = yami.Parameters()
        params["output"] = int(self.id)
        params["state"] = int(self.wanted_state)

        yagent.agent.send_one_way(discovery.get(self.service), self.service,
                        "set_output_state", params);

    def on_state_change(self, state):
        if not self.ready or state != self.state:
            self.ready = True
            logging.debug("'%s' state changed %d->%d", self.name, self.state, state)
            self.state = state
            for c in self.callbacks:
                c()
            if self.wanted_state != self.state:
                logging.debug("Wanted state of '%s' different from actual, setting to %d", self.name, self.wanted_state)
                self.set_state()

    def subscribe(self, callback):
        self.callbacks.append(callback)
        callback()
