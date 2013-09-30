#include <windows.h>
#include <stdio.h>

__declspec(dllexport)
BOOL WINAPI
DllMain (HANDLE hDll, DWORD dwReason, LPVOID lpReserved)
{
	switch (dwReason)
	{
		case DLL_PROCESS_ATTACH:
			// Code to run when the DLL is loaded
			//printf("moflow_test_dll loaded\n");
			break;

		case DLL_PROCESS_DETACH:
			// Code to run when the DLL is freed
			break;

		case DLL_THREAD_ATTACH:
			// Code to run when a thread is created during the DLL's lifetime
			break;

		case DLL_THREAD_DETACH:
			// Code to run when a thread ends normally.
			break;
	}
	return TRUE;
}