#!/usr/bin/env python
# This Python file uses the following encoding: utf-8

#import logging
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
    },
    {
        'name': "Test temperature",
        'service': "io.test",
        'id': 1
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
    if inputs.inputs["Temperatura salon"].get()[1] > 21.5 or inputs.inputs["Temperatura pole"].get()[1] > inputs.inputs["Temperatura salon"].get()[1]:
        outputs.outputs["Kocioł grzanie"].set(0)
    elif inputs.inputs["Temperatura salon"].get()[1] < 21 and inputs.inputs["Temperatura pole"].get()[1] < 21:
        outputs.outputs["Kocioł grzanie"].set(1)

def heating_off():
    outputs.outputs["Kocioł grzanie"].set(0)

def heating_on():
    outputs.outputs["Kocioł grzanie"].set(1)

def circulation_auto():
    now = inputs.inputs["Timer"].get()
    minutes = now.hour * 60 + now.minute
    if 0 <= now.weekday() <= 4:
        if (345 <= minutes < 420) or (960 <= minutes < 1380):
            outputs.outputs["Pompa cyrkulacji CWU"].set(1)
        else:
            outputs.outputs["Pompa cyrkulacji CWU"].set(0)
    else:
        if 420 <= minutes < 1380:
            outputs.outputs["Pompa cyrkulacji CWU"].set(1)
        else:
            outputs.outputs["Pompa cyrkulacji CWU"].set(0)
    pass

def circulation_off():
    outputs.outputs["Pompa cyrkulacji CWU"].set(0)

def circulation_on():
    outputs.outputs["Pompa cyrkulacji CWU"].set(1)


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
        "name": "Ogrzewanie włączone",
        "rule": heating_on,
        "inputs": [
        ],
        "outputs": [
            "Kocioł grzanie"
        ]
    },
    {
        "name": "Cyrkulacja automatyczna",
        "rule": circulation_auto,
        "inputs": [
            "Timer"
        ],
        "outputs": [
            "Pompa cyrkulacji CWU"
        ]
    },
    {
        "name": "Cyrkulacja wylaczona",
        "rule": circulation_off,
        "inputs": [
        ],
        "outputs": [
            "Pompa cyrkulacji CWU"
        ]
    },
    {
        "name": "Cyrkulacja włączona",
        "rule": circulation_on,
        "inputs": [
        ],
        "outputs": [
            "Pompa cyrkulacji CWU"
        ]
    }
]

# services list
services = [
    {
        "name": "Ogrzewanie",
        "displays": [
            {
                "name": "Temperatura na zewnątrz",
                "data": {
                    "type": "temperature",
                    "from": "Temperatura pole"
                }
            },
            {
                "name": "Temperatura w salonie",
                "data": {
                    "type": "temperature",
                    "from": "Temperatura salon"
                }
            },
            {
                "name": "Ogrzewanie",
                "data": {
                    "type": "state",
                    "from": "Kocioł grzanie"
                }
            }
        ],
        "settings": [
            {
                "name": "Tryb ogrzewania",
                "data": {
                    "type": "switch",
                    "default": "automatyczne",
                    "values": {
                        "wyłączone": {
                            "rule": "Ogrzewanie wylaczone"
                        },
                        "automatyczne": {
                            "rule": "Ogrzewanie automatyczne"
                        },
                        "włączone": {
                            "rule": "Ogrzewanie włączone"
                        }
                    }
                }
            }
        ]
    },
    {
        "name": "Cyrkulacja CWU",
        "displays": [
            {
                "name": "Cyrkulacja",
                "data": {
                    "type": "state",
                    "from": "Pompa cyrkulacji CWU"
                }
            }
        ],
        "settings": [
            {
                "name": "Tryb cyrkulacji",
                "data": {
                    "type": "switch",
                    "default": "automatyczna",
                    "values": {
                        "wyłączona": {
                            "rule": "Cyrkulacja wylaczona"
                        },
                        "automatyczna": {
                            "rule": "Cyrkulacja automatyczna"
                        },
                        "włączona": {
                            "rule": "Cyrkulacja włączona"
                        }
                    }
                }
            }
        ]
    },
    {
        "name": "Test service",
        "displays": [
            {
                "name": "Testowe wejście temperatury",
                "data": {
                    "type": "temperature",
                    "from": "Test temperature"
                }
            }
        ],
        "settings": [
        ]
    }
]
