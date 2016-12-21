#!/usr/bin/env python

import socket
import struct
import yami

ip = [(s.connect(('8.8.8.8', 80)), s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, socket.SOCK_DGRAM)]][0][1]
agent = yami.Agent()
endpoint = agent.add_listener("tcp://" + ip + ":*")
