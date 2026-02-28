int sum(int a, int b) {
    return a + b;
}

int main() {
    int x = 5;
    int y = 10;
    {int no = 10;}
    int pickme = no;
    int result1 = 10 + sum(4, sum(4, sum(4, sum(5,5))));
    int result2 = 10 + sum(x, 4);
    int result3 = sum(x, x);
    return 0;
}