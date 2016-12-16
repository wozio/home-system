import json
import unittest
import string
import random
import sys
from websocket import create_connection

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
    return ws, result[u"params"][u"client_id"]

def randomString(size=6, chars=string.ascii_uppercase + string.digits + string.ascii_lowercase):
    return ''.join(random.choice(chars) for _ in range(size))

class TestStringMethods(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.ws1, cls.id1 = connectAndLogin()
        #cls.ws2, cls.id2 = connectAndLogin()

    @classmethod
    def tearDownClass(cls):
        cls.ws1.close()
        cls.ws2.close()

    def test_big_string(self):
        msg = json.dumps({
            'message': 'test_msg',
            'source': self.id1,
            #'target': self.id2,
            'parameters':{
                'test_string': randomString(5000)
            }
        })
        self.ws1.send(msg)
        #while(True):
            #recv_msg = json.loads(self.ws2.recv())
            #print recv_msg

        self.assertEqual('foo'.upper(), 'FOO')

    def test_isupper(self):
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_split(self):
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)

if __name__ == '__main__':
    unittest.main()