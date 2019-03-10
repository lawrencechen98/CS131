from config import API_KEY, SERVER_TO_PORT, HERD_CONNECTIONS, PLACES_URL
import asyncio


async def test():
	reader, writer = await asyncio.open_connection('127.0.0.1', SERVER_TO_PORT['Welsh'])
	writer.write("IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997".encode())
	writer.write_eof()
	data = await reader.read(n=-1)
	print('Received: {}\n'.format(data.decode()))
	writer.close()

async def get_client():
	reader, writer = await asyncio.open_connection('127.0.0.1', SERVER_TO_PORT['Hands'])
	writer.write("WHATSAT kiwi.cs.ucla.edu 10 5".encode())
	writer.write_eof()
	data = await reader.read(n=-1)
	print('Received: {}\n'.format(data.decode()))
	writer.close()

async def main():
	await asyncio.gather(test())
	await asyncio.sleep(1)
	await get_client()

if __name__ == '__main__':
	asyncio.run(main())