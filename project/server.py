from config import API_KEY, SERVER_TO_PORT, HERD_CONNECTIONS, PLACES_URL

import os
import asyncio
import sys
import time
import re
import aiohttp
import json

clients = {}

lat_long_matcher = re.compile(r'^[+-]\d*\.?\d+[+-]\d*\.?\d+$')

class Client:
	def __init__(self, client_id, location, client_time, time_diff, server):
		self.id = client_id
		self.location = location
		self.client_time = client_time
		self.time_diff = time_diff
		self.last_server_contacted = server

def isfloat(value):
	try:
		float(value)
		return True
	except ValueError:
		return False

async def main(name):
	global server_name 
	server_name = name
	open('{}.txt'.format(server_name), 'w+')
	server = await asyncio.start_server(handle_connection, host='127.0.0.1', port=SERVER_TO_PORT[server_name])
	await server.serve_forever()

async def write_message(msg, writer):
	try:
		writer.write(msg.encode())
		await writer.drain()
	except AttributeError:
		with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Could not send message \"{}\"\n'.format('ERROR', msg))
				f.flush()    
	writer.close()

async def write_unrecognized(msg, writer):
	await write_message('? {}'.format(msg), writer)
	with open('{}.txt'.format(server_name), 'a+') as f:
		f.write('{}: ? {}\n'.format('OUTPUT', msg))
		f.flush()

async def write_log_output(msg, writer):
	await write_message(msg, writer)
	with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: {}\n'.format('OUTPUT', msg))
				f.flush()

async def flood_client_update(msg, propagated=[]):
	flooded_servers = propagated + HERD_CONNECTIONS[server_name]
	msg = msg + " " + server_name + " " + ",".join(flooded_servers)
	
	for server in HERD_CONNECTIONS[server_name]:
		if server in propagated:
			continue
		try:
			reader, writer = await asyncio.open_connection('127.0.0.1', SERVER_TO_PORT[server])
			with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Opened new connection to {} server.\n'.format('NEW CONNECTION', server))
				f.flush()
		except:
			with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Could not connect to {} server.\n'.format('ERROR', server))
				f.flush()
			continue
		await write_log_output(msg, writer)
		with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Dropped connection to {} server.\n'.format('DROPPED CONNECTION', server))
				f.flush()

async def handle_iamat(msg, msg_data, time_received, writer):
	if len(msg_data) == 4 and lat_long_matcher.match(msg_data[2]) and isfloat(msg_data[3]) and float(msg_data[3]) >= 0:
		timediff = time_received-float(msg_data[3])
		timediff = '+' + str(timediff) if timediff >= 0 else '-' + str(timediff)
		client = Client(msg_data[1], msg_data[2], msg_data[3], timediff, server_name)
		
		response = 'AT %s %s %s %s %s' % (client.last_server_contacted, client.time_diff, client.id, client.location, client.client_time)
		
		if client.id not in clients or float(msg_data[3]) >= float(clients[client.id].client_time):
			clients[client.id] = client
			await write_log_output(response + "\n", writer)
			
			with open('{}.txt'.format(server_name), 'a+') as f:
					f.write('{}: {}\n'.format('FLOODING SERVERS', response))
					f.flush()
			asyncio.create_task(flood_client_update(response, [server_name]))
		else:
			await write_log_output(response + "\n", writer)
	else:
		await write_unrecognized(msg, writer)

async def handle_at(msg, msg_data, writer):
	if len(msg_data) == 8:
		connected_server = msg_data[-2]
		with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Opened new connection to {} server.\n'.format('NEW CONNECTION', connected_server))
				f.write('{}: {}\n'.format('INPUT', msg))
				f.write('{}: Dropped connection to {} server.\n'.format('DROPPED CONNECTION', connected_server))
				f.flush()

		client = Client(msg_data[3], msg_data[4], msg_data[5], msg_data[2], msg_data[1])
		if client.id not in clients or float(msg_data[5]) >= float(clients[client.id].client_time):
			clients[client.id] = client

			propagated = msg_data[-1].split(',')
			message = " ".join(msg_data[:-2])
			with open('{}.txt'.format(server_name), 'a+') as f:
					f.write('{}: {}\n'.format('FLOODING SERVERS', message))
					f.flush()
			asyncio.create_task(flood_client_update(message, propagated))
	else:
		await write_unrecognized(msg, writer)

def format_location(location):
	location_list = list(filter(lambda x: len(x) > 0, re.split(r'(\d*\.?\d+)', location)))
	negate = False
	lat_long_list = []
	for token in location_list:
		if token == '-':
			negate = True
			continue
		if isfloat(token) and negate:
			negate = False
			lat_long_list.append('-' + token)
		elif isfloat(token):
			lat_long_list.append(token)
	return lat_long_list[0] + ',' + lat_long_list[1]

async def get_places(params):
	async with aiohttp.ClientSession() as session:
			async with session.get(PLACES_URL, params=params) as resp:
				response_json = await resp.json()
				return response_json

async def handle_whatsat(msg, msg_data, writer):
	if (len(msg_data) == 4 
		and msg_data[1] in clients 
		and msg_data[2].isdigit() and int(msg_data[2]) <= 50 
		and msg_data[3].isdigit() and int(msg_data[3]) <= 20):
		
		client = clients[msg_data[1]]
		radius = int(msg_data[2]) * 1000
		location = format_location(client.location)
		num_items = int(msg_data[3])
		params = [('key', API_KEY), ('location', location), ('radius', radius)]

		with open('{}.txt'.format(server_name), 'a+') as f:
				f.write('{}: Location {}; Radius {}\n'.format('CALLING PLACES API', location, radius))
				f.flush()
		response_json = await get_places(params)
		response_json['results'] = response_json['results'][:num_items]
		response_formatted = json.dumps(response_json, indent=3)
		
		response = 'AT %s %s %s %s %s\n%s\n\n' % (client.last_server_contacted, client.time_diff, client.id, client.location, client.client_time, response_formatted)
		await write_log_output(response, writer)
	else:
		await write_unrecognized(msg, writer)

async def handle_connection(reader, writer):
	data = await reader.read(n=-1)
	time_received = time.time()
	msg = data.decode()
	msg_data = msg.strip().split()
	
	if msg_data[0] != 'AT':
		with open('{}.txt'.format(server_name), 'a+') as f:
			f.write('{}: {}\n'.format('INPUT', msg))
			f.flush()

	if msg_data[0] == 'IAMAT':
		await handle_iamat(msg, msg_data, time_received, writer)
	elif msg_data[0] == 'AT':
		await handle_at(msg, msg_data, writer)
	elif msg_data[0] == 'WHATSAT':
		await handle_whatsat(msg, msg_data, writer)
	else:
		await write_unrecognized(msg, writer)

if __name__ == '__main__':
	if len(sys.argv) != 2:
		print('Usage: server.py SERVER_NAME.')
		sys.exit(1)

	if sys.argv[1] not in SERVER_TO_PORT:
		print('SERVER_NAME not valid.')
		sys.exit(1)

	asyncio.run(main(sys.argv[1]))