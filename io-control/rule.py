#!/usr/bin/env python

import logging
import inputs

class rule:
    def __init__(self, name, human_name, rule_callback, input_list):
        self.name = name
        self.human_name = human_name
        self.rule_callback = rule_callback

        # subscribe for input changes, for each input change rule_callback will be called
        for i in input_list:
            inputs.inputs[i].subscribe(self.on_input_change)

    def on_input_change(self):
        self.rule_callback
