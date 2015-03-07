int f(int n, int step, int max) {
  int nums[n];
  for (int i = 0; i < n; i++) {
    nums[i] = (i * step) % max;
  }
  int minVal, maxVal;
  minVal = maxVal = nums[0];
  for (int i = 1; i < n; i++) {
    int x = nums[i];
    minVal = (x < minVal ? x : minVal);
    maxVal = (x > maxVal ? x : maxVal);
  }
  for (int i = 0; i < n; i++) {
    nums[i] -= (maxVal - minVal) / 2;
  }
  return nums[0];
}

int main() {
  return f(10000, 17, 17777);
}
