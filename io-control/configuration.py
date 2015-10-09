#!/usr/bin/env python

import logging

# inputs definitions
inputs = [
    {
        'name': "temperatura_salon",
        'service': "io.1wire",
        'id': -6052829097502393072
    }
]

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
            "temperatura_salon"
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
