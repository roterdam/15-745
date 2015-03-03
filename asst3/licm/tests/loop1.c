int main() {
  int x = 0;
  for (int i = 0; i < 1000; i++) {
    int z = 4 * 1000;
    int y = i + z;
    x += i + z;
  }
  return x;
}
