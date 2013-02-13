# coding=utf-8

import ssl
import json
import socket
import struct
import threading

def build_packet(payload):

        # build packet to be sent over SSL connection
        fmt = "!cH{0:d}s".format(len(payload))
        cmd = '\x01'
        packet = struct.pack(fmt, cmd, len(payload), payload)

        return packet

class SSLThread(threading.Thread):
		
	def run(self):

		certfile = '/path/to/certificate.pem'
        	keyfile = '/path/to/key.pem'

        	server_address = ('localhost', 5555)

		# Connect to server
		s = socket.socket()
        	sock = ssl.wrap_socket(s, ssl_version=ssl.PROTOCOL_SSLv3, certfile=certfile, keyfile=keyfile)
		
		try:
        		sock.connect(server_address)
			
    		except socket.error as se:
        		sock.close()

	        # Send a packet:
		payload = {"update": {"name" : "player1", "position" : {"x" : 1, "y" : 1, "dir" : "n"}}}
        	packet = build_packet(json.dumps(payload))
        	sock.write(packet)

      		# Close the connection
      		sock.close()

if __name__ == '__main__':

	max_conn = 100

	# Let's spawn N clients:
	for x in xrange (max_conn):
  		SSLThread().start()

