int f(int n) {
  int x = 0;
  int y = 0;
  for (int i = 0; i < n; i++) {
    x += i;
    y += i;
  }
  return x;
}

int main() {
  return f(10000);
}
