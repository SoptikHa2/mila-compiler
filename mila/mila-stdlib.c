#include <stdio.h>

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
    (*k)--;
}
// TODO: comeFrom
