#!/usr/bin/env python

# outputs list: name, service, id
outputs = [
]

# inputs list
inputs = [
]

# events list: name, type, what to run (action or condition), data

events = [
    {
        "name": "weekly_timer_on",
        "type": "weeklytimer",
        "run": "on_weekly_timer_on",
        "data": {
            "timers": [
                { "day":0, "hour":"06:00" },
                { "day":0, "hour":"16:00" },
                { "day":1, "hour":"06:00" },
                { "day":1, "hour":"16:00" },
                { "day":2, "hour":"06:00" },
                { "day":2, "hour":"16:00" },
                { "day":3, "hour":"06:00" },
                { "day":3, "hour":"16:00" },
                { "day":4, "hour":"06:00" },
                { "day":4, "hour":"16:00" },
                { "day":5, "hour":"06:00" },
                { "day":6, "hour":"06:30" },
            ]
        }
    },
    {
        "name": "weekly_timer_off",
        "type": "weeklytimer",
        "run": "on_weekly_timer_off",
        "data": {
            "timers": [
                { "day":0, "hour":"07:15" },
                { "day":0, "hour":"23:00" },
                { "day":1, "hour":"07:15" },
                { "day":1, "hour":"23:00" },
                { "day":2, "hour":"07:15" },
                { "day":2, "hour":"23:00" },
                { "day":3, "hour":"07:15" },
                { "day":3, "hour":"23:00" },
                { "day":4, "hour":"07:15" },
                { "day":4, "hour":"23:00" },
                { "day":5, "hour":"23:00" },
                { "day":6, "hour":"23:00" },
            ]
        }
    }
]

# conditions list: name, type, data, what to run (action or condition for two cases: when returning true or false)

# actions list: name, type, data

actions = [
    {
        "name": "on_weekly_timer_on",
        "type": "set_output",
        "data":
        {
            "output": "rb1",
            "state": 1
        }
    },
    {
        "name": "on_weekly_timer_off",
        "type": "set_output",
        "data":
        {
            "output": "rb1",
            "state": 0
        }
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
