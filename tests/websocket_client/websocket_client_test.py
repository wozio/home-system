import json
import unittest
import string
import random
import sys
import threading
import time
from websocket import create_connection

threads = []
sendThreads = []

numOfMessages = 10
sizeOfString = 100

def connectAndLogin():
    # web sockets creating
    ws = create_connection("ws://localhost:8080/access/client")
    # logging in
    login_msg = json.dumps({
        'message': 'login',
        'sequence_number': 0,
        'source': "test_source",
        'target': 'control-server',
        'parameters':{
            'email': "test@test",
            'password': "test"
        }
    })
    ws.send(login_msg)
    result = json.loads(ws.recv())
    if result[u"result"] != u"success":
        raise Exception("Unable to login: " + result[u"reason"])
    print "logged in " + result[u"params"][u"client_id"]
    
    return ws, result[u"params"][u"client_id"]

def receiveThreadExec(ws):
    lastMsg = u"test_msg_" + str(numOfMessages - 1)
    while(True):
        s = ws.recv()
        #print sys.getsizeof(s)
        recv_msg = json.loads(s)
        print "Received ->" + recv_msg[u"target"] + " " + recv_msg[u"message"]
        if recv_msg[u"message"] == lastMsg:
            break
    print "Receive thread ended"

def randomString(size=6, chars=string.ascii_uppercase + string.digits + string.ascii_lowercase):
    return ''.join(random.choice(chars) for _ in range(size))

def sendThreadExec(ws, fromId, toId):
    print "Send " + fromId + "->" + toId
    s = randomString(sizeOfString)
    for i in range(0, numOfMessages):
        msg = json.dumps({
            'message': 'test_msg_'+str(i),
            'source': fromId,
            'target': toId,
            'parameters':{
                'test_string': s
            }
        })
        ws.send(msg)
    print "End of sending " + fromId + "->" + toId

class TestStringMethods(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.ws1, cls.id1 = connectAndLogin()
        cls.ws2, cls.id2 = connectAndLogin()

    @classmethod
    def tearDownClass(cls):
        cls.ws1.close()
        cls.ws2.close()

    def test_big_string(self):
        thread = threading.Thread(target=receiveThreadExec, args=[self.ws1])
        thread.start()
        threads.append(thread)

        thread = threading.Thread(target=receiveThreadExec, args=[self.ws2])
        thread.start()
        threads.append(thread)

        thread = threading.Thread(target=sendThreadExec, args=[self.ws1, self.id1, self.id2])
        thread.start()
        sendThreads.append(thread)

        thread = threading.Thread(target=sendThreadExec, args=[self.ws2, self.id2, self.id1])
        thread.start()
        sendThreads.append(thread)

        for t in sendThreads:
            t.join()
        print "Send threads ended"
        for t in threads:
            t.join()
        print "Receive threads ended"

if __name__ == '__main__':
    unittest.main()