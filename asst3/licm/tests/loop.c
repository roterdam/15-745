int f(int n) {
  int sum = 0;
  for (int i = 0; i < n - 1; i++) {
    for (int j = 0; j < n - i; j++) {
      sum += n / 7;
    }
  }
  return sum;
}

int main() {
  return f(1000);
}
