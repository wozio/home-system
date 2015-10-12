#!/usr/bin/env python

import logging
import inputs
import outputs

# inputs definitions
defined_inputs = [
    {
        'name': "Temperatura salon",
        'service': "io.1wire",
        'id': -6052829097502393072
    },
    {
        'name': "Temperatura pole",
        'service': "io.1wire",
        'id': 8358689710083321104
    }
]

# outputs definitions
defined_outputs = [
    {
        'name': "Kociol grzanie",
        'service': 'io.relay-board',
        'id': 0
    }
]

# rules definitions
def heating_auto():
    try:
        if inputs.inputs["Temperatura salon"].get() > 21.5 or inputs.inputs["Temperatura pole"].get() > inputs.inputs["Temperatura salon"].get():
            outputs.outputs["Kociol grzanie"].set(0)
        elif inputs.inputs["Temperatura salon"].get() < 21 and inputs.inputs["Temperatura pole"].get() < 21:
            outputs.outputs["Kociol grzanie"].set(1)
    except KeyError:
        logging.warn("Input or output has not been found, something is wrong with configuration...")
    except RuntimeError:
        logging.warn("Something is not ready yet")

# rules list
rules = [
    {
        "name": "Ogrzewanie auto",
        "rule": heating_auto,
        "inputs": [
            "Temperatura salon",
            "Temperatura pole"
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
