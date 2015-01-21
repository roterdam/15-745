int isPrime(int n) {
  for (int guess = 2; guess < n; guess++) {
    if (n % guess == 0) {
      return 0;
    }
  }
  return 1;
}

int nthprime(int n) {
  int current, numFound;
  current = -1;
  numFound = 0;
  while (numFound < n) {
    current += 1;
    if (isPrime(current)) {
      numFound += 1;
    }
  }
  return current;
}
