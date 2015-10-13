#!/usr/bin/env python

import logging
import inputs
import outputs

class rule:
    def __init__(self, name, rule_callback, input_list, output_list):
        logging.debug("Created rule '%s'", name)
        self.enabled = False
        self.name = name
        self.rule_callback = rule_callback

        # subscribe for input/output changes, for each change run_rule will be called
        for i in input_list:
            try:
                logging.debug("Rule '%s' subscribing to input '%s'", name, inputs.inputs[i].name)
                inputs.inputs[i].subscribe(self.run_rule)
            except KeyError:
                logging.warn("Rule '%s' unable to subscribe for input '%s'. Rule will not be trigerred by this input change", name, i)
                
        for i in output_list:
            try:
                logging.debug("Rule '%s' subscribing to output '%s'", name, outputs.outputs[i].name)
                outputs.outputs[i].subscribe(self.run_rule)
            except KeyError:
                logging.warn("Rule '%s' unable to subscribe for output '%s'. Rule will not be trigerred by this output change", name, i)

    def run_rule(self):
        if self.enabled:
            try:
                self.rule_callback()
            except KeyError:
                logging.error("Rule '%s': Input or output has not been found, something is wrong with configuration...", self.name)
            except RuntimeError:
                logging.warn("Rule '%s': Something is not ready yet", self.name)
            
    def enable(self):
        logging.debug("Enabling rule '%s'", self.name)
        self.enabled = True
        self.run_rule()
        
    def disable(self):
        logging.debug("Disabling rule '%s'", self.name)
        self.enabled = False
