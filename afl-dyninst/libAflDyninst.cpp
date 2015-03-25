#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <vector>
#include <algorithm>
#include "config.h"
#include <sys/types.h>
#include <sys/shm.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;


static u8* trace_bits;
static s32 shm_id;                    /* ID of the SHM region             */
static int __afl_temp_data;
static pid_t __afl_fork_pid;
static unsigned short prev_id;


void initAflForkServer()
{
    char *shm_env_var = getenv(SHM_ENV_VAR);
    if(!shm_env_var) {
        printf("Error getting shm\n");
        return;
    }
    shm_id = atoi(shm_env_var);
    trace_bits = (u8*)shmat(shm_id, NULL, 0);
    if(trace_bits == (u8*)-1) {
        perror("shmat");
        return;
    }

    // enter fork() server thyme!
    int n = write(FORKSRV_FD+1, &__afl_temp_data,4);
    if( n!=4 ) {
        printf("Error writting fork server\n");
        return;
    }
    while(1) {
        n = read(FORKSRV_FD,&__afl_temp_data,4);
        if(n != 4) {
            printf("Error reading fork server %x\n",__afl_temp_data);
            return;
        }

        __afl_fork_pid = fork();
        if(__afl_fork_pid < 0) {
            printf("Error on fork()\n");
            return;
        }
        if(__afl_fork_pid == 0) {
            close(FORKSRV_FD);
            close(FORKSRV_FD+1);
            break;
        } else {
            // parrent stuff
            n = write(FORKSRV_FD+1,&__afl_fork_pid, 4);
            pid_t temp_pid = waitpid(__afl_fork_pid,&__afl_temp_data,2);
            if(temp_pid == 0) {
                return;
            }
            n = write(FORKSRV_FD+1,&__afl_temp_data,4);
        }

    }

}


// Should be called on basic block entry
void bbCallback(unsigned short id)
{
    if(trace_bits) {
        trace_bits[prev_id ^ id]++;
        prev_id = id >> 1;
    }
}
