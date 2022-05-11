#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/shm.h>
#include <pthread.h>

void write(int param) {
    printf("%d", param);
}
void writed(double param) {
    printf("%lf", param);
}
void writes(char * param) {
    printf("%s", param);
}

void writeln(int param) {
    printf("%d\n", param);
}
void writelnd(double param) {
    printf("%lf\n", param);
}
void writelns(char * param) {
    printf("%s\n", param);
}

int readln(int * k) {
    return scanf("%d", k);
}
int readlnd(double * k) {
    return scanf("%lf", k);
}

int conv_int(double d) {
    return (int)d;
}
double conv_dbl(int i) {
    return (double)i;
}

void dec(int * k){
    (*k)--;
}
void inc(int * k){
    (*k)++;
}

int __INTERNAL_segment_id;
int * __INTERNAL_sharedData = NULL;
void setupSharedInts(int cnt) {
    __INTERNAL_segment_id = shmget(IPC_PRIVATE, sizeof(int)*(cnt), IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR);
    __INTERNAL_sharedData = (int *) shmat(__INTERNAL_segment_id, 0, 0);
}
int getSharedInt(int offset) {
    return __INTERNAL_sharedData[offset];
}
void setSharedInt(int offset, int value) {
    __INTERNAL_sharedData[offset] = value;
}
int getAndIncrement(int offset) {
    return (*(_Atomic int *)((__INTERNAL_sharedData+offset)))++;
}
void trashSharedInts() {
    shmdt(__INTERNAL_sharedData);
    shmctl(__INTERNAL_segment_id, IPC_RMID, 0);
}

// fork n-times
int comeFrom(int n) {
    for(int i = 0; i < n-1; i++) {
        int forkRes = fork();
        if (forkRes == 0) return i;
    }
    return n-1;
}
