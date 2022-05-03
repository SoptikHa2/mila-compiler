#include <stdio.h>

void write(int param) {
    printf("%d", param);
}
void writeln(int param) {
    printf("%d\n", param);
}
int readln() {
    int k;
    scanf("%d", &k);
    return k;
}
void dec(int * k){
    (*k)--;
}
// TODO: comeFrom
