#!/usr/bin/env python

import logging
import input
import yami
import yagent
import discovery

# output has the same functionality as input with added possibility to set its value

class output(input.input):

    def __init__(self, name, service, id):
        super(output, self).__init__(name, service, id)
        self.wanted_value = None

    def set(self, value):
        if value != self.wanted_value:
            self.wanted_value = value
            logging.debug("'%s' wanted value set to %s", self.name, str(value))

        if self.wanted_value != self.value and self.wanted_value != None:
            self.set_value()

    def set_value(self):
        if self.state == input.state_ok:
            params = yami.Parameters()
            params["id"] = int(self.id)
            params["value"] = int(self.wanted_value)
    
            yagent.agent.send_one_way(discovery.get(self.service), self.service,
                            "set_output_state", params)
