#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int gcd(int a, int b) {
    int q, r;

    while (b) {
        q = a/b;
        r = a%b;
        a = b;
        b = r;
    }
    return a;
}

long lcm(int a, int b) {
    long ans = a;
    ans /= gcd(a,b);
    ans *= b;
    return ans;
}

void shuffle(int* A, int n) {
    int i,j,k,t;
    k = n;
    for (i = 0; i < n; i++) {
        j = rand() % k;
        t = A[j]; A[j] = A[k-1]; A[k-1] = t;
        k--;
    }
}

struct Test {
    int* A;
    int* B;
    int n;
};

int runtest(struct Test* t, int verbose) {
    char c;
    int* A = t->A;
    int* B = t->B;
    int n = t->n;
    int i;
    int status = 1;
    int requests = 0;
    fprintf(stderr, "check:\n");
    for (i = 0; i < n; i++) {
        fprintf(stderr, "%d ", A[i]);
    }
    fprintf(stderr, "\n");

    printf("%d\n", n); // size

    while (1) {
        if (verbose) { fprintf(stderr, "wait command\n"); }
        scanf("%c", &c);
        if (c == '\n') {
            continue;
        }
        if (verbose) { fprintf(stderr, "cmd: '%c'\n", c); }
        if (c == '!') {
            for (i = 0; i < n; i++) {
                scanf("%d", &B[i]);
            }
            if (memcmp(A, B, n*sizeof(int)) !=0) {
                fprintf(stderr, "got: \n");
                for (i = 0; i < n; i++) {
                    fprintf(stderr, "%d ", B[i]);
                }
                fprintf(stderr, "\n");
                fprintf(stderr, "FAIL\n");
                exit(1);
                break;
            } else {
                if (verbose) {
                    fprintf(stderr, "got: \n");
                    for (i = 0; i < n; i++) {
                        fprintf(stderr, "%d ", B[i]);
                    }
                    fprintf(stderr, "\n");
                }
                fprintf(stderr, "OK\n");
                status = 1;
                break;
            }
        } else if (c == '?') {
            int a, b;
            long l;
            requests ++;
            if (requests > n + 5000) {
                fprintf(stderr, "FAIL to many requests: %d\n", requests);
                exit(1);
            }
            scanf("%d%d", &a, &b);
            if (verbose) { fprintf(stderr, "check: %d %d (%d, %d)\n", a, b, A[a-1], A[b-1]); }
            l = lcm(A[a-1],A[b-1]);
            if (verbose) { fprintf(stderr, "lcm: %ld\n", l); }
            printf("%ld\n", l);
        } else {
            fprintf(stderr, "unknown cmd: '%d'\n", c);
            break;
        }
    }
    return status;
}

int main(int argc, char** argv) {
    int n, i, j, k, l;
    char* sep = " ";
    char* tok;
    struct Test tests[10000];
    int ntests = 0;
    int t;
    int verbose = 0;

    setbuf(stderr, NULL);
    setbuf(stdout, NULL);

    // --multi [10]
    // 10 random test cases
    // --single n '1 2 3 4 5'

    for (i = 0; i < argc; i++) {
        if (!strcmp(argv[i], "--single")) {
            if (argc - i < 3) {
                fprintf(stderr, "./Ftest --single n '1 2 3 4 5'\n");
                return 1;
            }
            n = atoi(argv[i+1]);
            if (n <= 2) {
                fprintf(stderr, "n must be >= 3\n");
                return 1;
            }
            fprintf(stderr, "single mode\n");
            tests[ntests].A = malloc(n*sizeof(int));
            tests[ntests].B = malloc(n*sizeof(int));
            tests[ntests].n = n;
            j = 0;
            for (tok = strtok(argv[i+2], sep); tok && i < n; tok = strtok(NULL, sep)) {
                tests[ntests].A[j++] = atoi(tok);
            }
            if (j != n) {
                fprintf(stderr, "wrong number of numbers: %d != %d\n", j, n);
                return 1;
            }
            ntests++;
            break;
        } else if (!strcmp(argv[i], "--multi")) {
            int fixedn = 0;
            if (argc - i < 2) {
                fprintf(stderr, "./Ftest --multi t\n");
                return 1;
            }
            t = atoi(argv[i+1]);
            if (t <= 0) {
                fprintf(stderr, "t must be > 0\n");
                return 1;
            }
            if (argc - i < 4) {
                n = atoi(argv[i+2]);
                fixedn = 1;
            }
            fprintf(stderr, "milti mode, tests: %d\n", t);
            for (j = 0; j < t; j++) {
                if (!fixedn) {
                    n = rand() % 97+3;
                }
                tests[j].A = malloc(n*sizeof(int));
                tests[j].B = malloc(n*sizeof(int));
                tests[j].n = n;
                l = rand() % 100+1;
                for (k = 0; k < n; k++) {
                    tests[j].A[k] = l+k;
                }
                shuffle(tests[j].A, n);
            }
            ntests = t;
            break;
        } else if (!strcmp(argv[i], "--verbose")) {
            verbose = 1;
        }
    }

    printf("%d\n", ntests); // number of tests
    for (i = 0; i < ntests; i++) {
        if (!runtest(&tests[i], verbose)) {
            break;
        }
    }

    for (i = 0; i < ntests; i++) {
        free(tests[i].A);
        free(tests[i].B);
    }
    return 0;
}
