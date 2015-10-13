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

# rules callbacks
def heating_auto():
    if inputs.inputs["Temperatura salon"].get() > 21.5 or inputs.inputs["Temperatura pole"].get() > inputs.inputs["Temperatura salon"].get():
        outputs.outputs["Kociol grzanie"].set(0)
    elif inputs.inputs["Temperatura salon"].get() < 21 and inputs.inputs["Temperatura pole"].get() < 21:
        outputs.outputs["Kociol grzanie"].set(1)
        
def heating_off():
    outputs.outputs["Kociol grzanie"].set(0)
    
def heating_on():
    outputs.outputs["Kociol grzanie"].set(1)

# rules list
rules = [
    {
        "name": "Ogrzewanie auto",
        "rule": heating_auto,
        "inputs": [
            "Temperatura salon",
            "Temperatura pole"
        ],
        "outputs": [
            "Kociol grzanie"
        ]
    },
    {
        "name": "Ogrzewanie wylaczone",
        "rule": heating_off,
        "inputs": [
        ],
        "outputs": [
            "Kociol grzanie"
        ]
    },
    {
        "name": "Ogrzewanie wlaczone",
        "rule": heating_on,
        "inputs": [
        ],
        "outputs": [
            "Kociol grzanie"
        ]
    }
]

# services list
services = [
    {
        "name": "Ogrzewanie",
        "settings": [
            {
                "name": "Tryb",
                "type": "switch",
                "data": {
                    "values":[
                        {
                            "value": "wyl",
                            "rule": "Ogrzewanie wylaczone"
                        },
                        {
                            "value": "auto",
                            "rule": "Ogrzewanie auto"
                        },
                        {
                            "value": "wl",
                            "rule": "Ogrzewanie wlaczone"
                        }
                    ]
                }
            }
        ]
    }
]
