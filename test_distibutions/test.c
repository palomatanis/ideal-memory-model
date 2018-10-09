#include <stdio.h>
#include <math.h>
#include "multinomial.h"
#include <gsl/gsl_cdf.h> 
#include <stdlib.h>

//  gcc -Wall test.c multinomial.c -lgsl -lm -o test

int main(int argc, char **argv) {

	double p, *ps, resM = 1, resB = 1;
	unsigned int *n, k, c, i, maxN;

	if (argc < 3) {
		printf("[!] Use: %s <unkown bits> <assoc> [maxN]\n", argv[0]);
		return 1;
	}
	if (!(c = atoi(argv[1]))) {
		printf("[!] error: invalid bits\n");
		return 1;
	}

	c = 1 << c;

	if (!(k = atoi(argv[2]))) {
		printf("[!] error: invalid assoc\n");
		return 1;
	}

	if (!(ps = (double*) calloc(c, sizeof(double)))) {
		printf("[!] error: alloc ps\n");
		return 1;
	}
	if (!(n = (unsigned int*) calloc(c, sizeof(unsigned int)))) {
		printf("[!] error: alloc nb\n");
		return 1;
	}

	if (argc > 3) {
		maxN = atoi(argv[3]);
	} else {
		maxN = 4096;	
	}

	p = 1/(float)c;

	for (i = 0; i < c; i++) {
		ps[i] = p;
		n[i] = k;
	}

	printf("P(C)=%f ASSOC=%d\n", p, k);
	printf("====================\n");
	printf("n, binomial, multinomial\n"); 

	i = k;
	while (resB > 0.0001 || resM > 0.0001) {
		resM = cdf_multinomial_P (c, i, ps, n);
		resB = gsl_cdf_binomial_P (k-1, p, i);
		printf("%d, %.03f, %.03f\n", i, 1-resB, 1-resM);
		i += k;
	}

	return 0;
}
  

  
