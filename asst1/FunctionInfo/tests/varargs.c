int foo(int x, ...) {
  return x;
}

int main() {
  return foo(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
}
