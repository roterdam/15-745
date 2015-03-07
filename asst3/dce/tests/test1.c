int foo(int x){
    x = x + 1;
    int a = x + 2;  // dead code
    int b = a - 3;  // dead code
    int c = a + b;  // dead code
    return x;
}

int main(){
    return 0;
}
