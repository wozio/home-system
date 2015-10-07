#!/usr/bin/env python

import logging

# rules definitions
def heating_auto():
    logging.debug("heating_auto rule executed")

# rules list
rules = [
    {
        "name": "heating_auto",
        "human_readable_name": "Ogrzewanie automatyczne",
        "rule": heating_auto,
        "inputs": [
            "io.1wire_-6052829097502393072"
        ]
    }
]

# services: name, list of settings: name, friendly name, type

services = [
    {
        "name": "heating",
        "settings": [
            {
                "type": "switch",
                "data": {
                    "values":[
                        {
                            "value": "off",
                            "action": "on_heating_off"
                        },
                        {
                            "value": "auto",
                            "action": "on_heating_auto"
                        },
                        {
                            "value": "on",
                            "action": "on_heating_on"
                        }
                    ]
                }
            }
        ]
    }
]
