# coding=utf-8

import ssl
import json
import socket
import struct

def build_packet(payload):
	# build packet to be sent over SSL connection
	fmt = "!cH{0:d}s".format(len(payload))
	cmd = '\x01' 
	msg = struct.pack(fmt, cmd, len(payload), payload)
	return msg

if __name__ == '__main__':
	certfile = '/path/to/certificate.pem'
        keyfile = '/path/to/key.pem'

	server_address = ('localhost', 5555)

	s = socket.socket()
	sock = ssl.wrap_socket(s, ssl_version=ssl.PROTOCOL_SSLv3, certfile=certfile, keyfile=keyfile)
	sock.connect(server_address)


	payload = {"update": {"name" : "player1", "position" : {"x" : 1, "y" : 1, "dir" : "n"}}}
	packet = build_packet(json.dumps(payload))
	sock.write(packet)
	
	sock.close()

