#ifndef _PIN_SYSCALLS_H
#define _PIN_SYSCALLS_H

/* Misc. definitions that are related to system calls */

/***************** Syscalls ***************/
// FIXME: use the ones from /usr/include/asm/unistd.h

#define __NR_nosyscall            0     
#define __NR_read			3
#define __NR_write		4
#define __NR_open			5
#define __NR_close			6
#define __NR_execve		 11
#define __NR_lseek               19
#define __NR_mmap		 90
#define __NR_socketcall	102
#define __NR_mmap2		192

// We can't allow mmap calls with (prot & PROT_WRITE)
#define _PROT_WRITE 0x02

// Windows system calls @ http://code.google.com/p/miscellaneouz/source/browse/trunk/winsyscalls?spec=svn26&r=26
// These numbers are for Windows 7
enum {
  __NR_closewin = 0x32,
  __NR_createfilewin = 0x42,
  __NR_createsectionwin = 0x54,
  __NR_mapviewofsectionwin = 0xa8,
  __NR_readfilewin = 0x111,
  __NR_writefilewin = 0x018c,
  __NR_setinfofilewin = 0x149,
};

#define _PAGE_EXECUTE_READWRITE 0x40
#define _PAGE_EXECUTE_WRITECOPY 0x80
#define _PAGE_READWRITE 0x04
#define _PAGE_WRITECOPY 0x08
#define _PAGE_WRITECOMBINE 0x400
#define _PROT_WRITE_WIN (_PAGE_EXECUTE_READWRITE | _PAGE_EXECUTE_WRITECOPY | \
                        _PAGE_READWRITE | _PAGE_WRITECOPY | _PAGE_WRITECOMBINE)

/********************************************/

// socket specific calls
#define _A1_socket     0x1
#define _A1_bind       0x2
#define _A1_listen     0x4
#define _A1_accept     0x5
#define _A1_send       0x9
#define _A1_recv       0xa
#define _A1_setsockopt 0xe

/********************************************/

/*************** Syscall Regs ***************/
#define SCOUTREG_WIN LEVEL_BASE::REG_EDX
#define SCOUTREG_LIN LEVEL_BASE::REG_EAX

#endif
