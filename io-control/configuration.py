#!/usr/bin/env python
# This Python file uses the following encoding: utf-8

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
        'name': "Kocioł grzanie",
        'service': 'io.relay-board',
        'id': 0
    },
    {
        'name': "Pompa cyrkulacji CWU",
        'service': 'io.relay-board',
        'id': 3
    }
]

# rules callbacks
def heating_auto():
    if inputs.inputs["Temperatura salon"].get() > 21.5 or inputs.inputs["Temperatura pole"].get() > inputs.inputs["Temperatura salon"].get():
        outputs.outputs["Kocioł grzanie"].set(0)
    elif inputs.inputs["Temperatura salon"].get() < 21 and inputs.inputs["Temperatura pole"].get() < 21:
        outputs.outputs["Kocioł grzanie"].set(1)

def heating_off():
    outputs.outputs["Kocioł grzanie"].set(0)

def heating_on():
    outputs.outputs["Kocioł grzanie"].set(1)

# rules list
rules = [
    {
        "name": "Ogrzewanie automatyczne",
        "rule": heating_auto,
        "inputs": [
            "Temperatura salon",
            "Temperatura pole"
        ],
        "outputs": [
            "Kocioł grzanie"
        ]
    },
    {
        "name": "Ogrzewanie wylaczone",
        "rule": heating_off,
        "inputs": [
        ],
        "outputs": [
            "Kocioł grzanie"
        ]
    },
    {
        "name": "Ogrzewanie wlaczone",
        "rule": heating_on,
        "inputs": [
        ],
        "outputs": [
            "Kocioł grzanie"
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
                    "default": 1,
                    "values":[
                        {
                            "value": "wyl",
                            "rule": "Ogrzewanie wylaczone"
                        },
                        {
                            "value": "auto",
                            "rule": "Ogrzewanie automatyczne"
                        },
                        {
                            "value": "wl",
                            "rule": "Ogrzewanie włączone"
                        }
                    ]
                }
            }
        ]
    }
]
