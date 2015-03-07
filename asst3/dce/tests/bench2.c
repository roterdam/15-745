int f(int n) {
  int a, b, c, x;
  a = b = c = x = 0;
  for (int i = 0; i < n; i++) {
    c += b;
    b += a;
    a += i;
    x += i;
  }
  return x;
}

int main() {
  return f(10000);
}
