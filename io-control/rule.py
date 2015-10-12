#!/usr/bin/env python

import logging
import inputs

class rule:
    def __init__(self, name, rule_callback, input_list):
        logging.debug("Created rule '%s'", name)
        self.name = name
        self.rule_callback = rule_callback

        # subscribe for input changes, for each input change rule_callback will be called
        for i in input_list:
            try:
                logging.debug("Subscribing to input '%s'", inputs.inputs[i].name)
                inputs.inputs[i].subscribe(self.on_input_change)
            except KeyError:
                logging.warn("Rule '%s' unable to subscribe for input '%s'. Rule will not be trigerred by this input change", name, i)
            except RuntimeError:
                logging.warn("Something is not ready yet")

    def on_input_change(self):
        self.rule_callback()
