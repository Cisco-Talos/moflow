#include <Windows.h>
#include "NetSock.h"

int test_stack_buffer_overflow(char *data);

void Client()
{
	char buf[1024];
	NetSock client;
	int ret = 0;

	if (!client.Connect("127.0.0.1", 1337))
		return;

	// Write some ASCII string.
	ret = client.Write((unsigned char*)"asdf", 4);
	if(ret <= 0)
	{
		printf("[client] write error \"asdf\": %d\n", WSAGetLastError());
		client.Disconnect();
		return;
	}

	// Read some ASCII string.
	//Sleep(500);
	ret = client.Read(buf, sizeof(buf));
	if(ret <= 0)
	{
		printf("[client] read error: %d\n", WSAGetLastError());
		client.Disconnect();
		return;
	}
	// Write out the string.
 
	test_stack_buffer_overflow(buf);

	client.Disconnect();
}

void Server()
{
	char buf[1024];
	int ret = 0;

	NetSock server;
	while(ret == false)
		ret = server.Listen(1337, "127.0.0.1");
	
	NetSock *remote = server.Accept();

	//Sleep(500);
	ret = remote->Read(buf, 4);
	/*
	if(strncmp(buf, "asdf", 4))
	{
		remote->Disconnect();
		return;
	}
	*/

	//Sleep(500);
	memset(buf, 'A', sizeof(buf));
	ret = remote->Write(buf, sizeof(buf));

	remote->Disconnect();
	server.Disconnect();
}

__declspec(dllexport) int dll_test_network_io()
{
	char buf[1024];

	printf(" test_network_io()\n");

	NetSock::InitNetworking();

    CreateThread(0, 0, (LPTHREAD_START_ROUTINE)Server, 0, 0, 0);
	Sleep(1000);
	Client();

	return 0;
}