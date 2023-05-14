#include <stdio.h>
#include <stdlib.h>

int main() {
    int t = 1000;
    int i,j;
    printf("%d\n", t);
    for (i = 0; i < t; ++i) {
        int k = 3+rand()%50;
        printf("%d\n", k);
        for (j = 0; j < k; j++) {
            int ch = 1+rand()%9;
            printf("%c", ch+'0');
        }
        printf("\n");
    }
    return 0;
}
