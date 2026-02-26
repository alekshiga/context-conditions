void func1() {
    return 5;
}

int func2() {
}

int func3(int a, int b) {
    return a + b;
}

int main() {
    func1();
    
    int x = func2();
    
    func3(1);
    func3(1, 2, 3);
    
    return 0;
}