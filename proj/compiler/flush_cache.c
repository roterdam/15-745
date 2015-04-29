#include <stdlib.h>
#include <stdio.h>

int main(){
    const int size = 1024*1024*1; // 1M > L1 + L2 cache sizes
    char *c = (char *) malloc(size);
    for (int i = 0; i < 0xffff; i++){
        for (int j = 0; j < size; j++){
            c[j] = i*j;
        }
    }
    return 0;
}
