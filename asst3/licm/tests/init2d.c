int f(int n) {
  float M[n][n];
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      M[i][j] = 1.0 / (i + 1);
    }
  }
  
  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) {
      M[i][j] += 1.0 / (j + 1);
    }
  }

  float sum = 0.0;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      sum += M[i][j];
    }
  }

  return (int)f;
}

int main() {
  return f(500);
}
