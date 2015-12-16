#!/usr/bin/env python

import logging
import yami
import yagent
import discovery

state_unknown = 0
state_ok = 1

class output:

    def __init__(self, output_name, output_service, output_id):
        self.id = output_id
        self.service = output_service
        self.name = output_name
        self.state = state_unknown
        self.value = None
        self.callbacks = []
        self.wanted_value = None

        logging.info("Created output '%s' with service=%s and id=%d", self.name, output_service, output_id)

    def get(self):
        if self.state != state_ok:
            raise RuntimeError("Output not OK")
        return self.value

    def set(self, value):
        if value != self.wanted_value:
            self.wanted_value = value
            logging.debug("'%s' state set to %f", self.name, value)

        if self.wanted_value != self.value:
            self.set_value()

    def set_value(self):
        if self.state == state_ok:
            params = yami.Parameters()
            params["id"] = int(self.id)
            params["value"] = int(self.wanted_value)
    
            yagent.agent.send_one_way(discovery.get(self.service), self.service,
                            "set_output_state", params);

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
            if self.wanted_value and self.wanted_value != self.value:
                logging.debug("Wanted value of '%s' different from actual, setting to %s", self.name, str(self.wanted_value))
                self.set_value()

    def subscribe(self, callback):
        self.callbacks.append(callback)
        callback()
