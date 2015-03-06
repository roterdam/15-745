int foo(int x){
    x = x + 1;
    int a = x + 2;
    int b = a - 3;
    int c = a + b;
    return 0;
}

int main(){
    return foo(3);
}
