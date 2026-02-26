int sum(int a, int b) {
    a = 10;
    return a + b;
}

int main() {
    int x = 5;
    int y = 10;
    int result = sum(x, y);
    sum(x, y) = 100;
    return 0;
}