#!/bin/bash

cat <<EOF | gcc -xc -c -o ./obj/tmp2.o -
int ret3() { return 3; }
int ret5() { return 5; }
int add(int x, int y) { return x+y; }
int sub(int x, int y) { return x-y; }
int add6(int a, int b, int c, int d, int e, int f) {
  return a+b+c+d+e+f;
}
EOF

assert() {
    expected="$1"
    input="$2"

    echo "Compiling $input"
    ./bin/jcc "$input" --emit-tokens --emit-ast > ./obj/tmp.s || exit
    gcc -static -o ./bin/tmp ./obj/tmp.s ./obj/tmp2.o
    ./bin/tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

make || exit

assert 0 'int main() { return 0; }'
assert 42 'int main() { return 42; }'
assert 21 'int main() { return 5+20-4; }'
assert 41 'int main() { return  12 + 34 - 5 ; }'
assert 47 'int main() { return 5+6*7; }'
assert 15 'int main() { return 5*(9-6); }'
assert 4 'int main() { return (3+5)/2; }'
assert 10 'int main() { return -10+20; }'
assert 10 'int main() { return - -10; }'
assert 10 'int main() { return - - +10; }'

assert 1 'int main() { return 42==42; }'
assert 1 'int main() { return 0!=1; }'
assert 0 'int main() { return 42!=42; }'
assert 0 'int main() { return 0==1; }'

assert 1 'int main() { return 0<1; }'
assert 0 'int main() { return 1<1; }'
assert 0 'int main() { return 2<1; }'
assert 1 'int main() { return 0<=1; }'
assert 1 'int main() { return 1<=1; }'
assert 0 'int main() { return 2<=1; }'

assert 1 'int main() { return 1>0; }'
assert 0 'int main() { return 1>1; }'
assert 0 'int main() { return 1>2; }'
assert 1 'int main() { return 1>=0; }'
assert 1 'int main() { return 1>=1; }'
assert 0 'int main() { return 1>=2; }'

assert 3 'int main() { int a = 0; a=3; return a; }'
assert 3 'int main() { int a = 3; return a; }'
assert 8 'int main() { int a; int z; a=3; z=5; return a+z; }'

assert 3 'int main() { int a; a=3; return a; }'
assert 8 'int main() { int a; int z; a=3; z=5; return a+z; }'
assert 6 'int main() { int a; int b; a=b=3; return a+b; }'
assert 3 'int main() { int foo=3; return foo; }'
assert 8 'int main() { int foo123; int bar; foo123=3; bar=5; return foo123+bar; }'
assert 6 'int main() { int _a_123; int _b_456; _a_123=_b_456=3; return _a_123+_b_456; }'

assert 1 'int main() { return 1; 2; 3; }'
assert 2 'int main() { 1; return 2; 3; }'
assert 3 'int main() { 1; 2; return 3; }'

assert 3 'int main() { {1; {2;} return 3;} }'
assert 5 'int main() { ;;; return 5; }'
assert 1 'int main() { return 1; return 2; }'

assert 3 'int main() { if (0) return 2; return 3; }'
assert 3 'int main() { if (1-1) return 2; return 3; }'
assert 2 'int main() { if (1) return 2; return 3; }'
assert 2 'int main() { if (2-1) return 2; return 3; }'
assert 4 'int main() { if (0) { 1; 2; return 3; } else { return 4; } }'
assert 3 'int main() { if (1) { 1; 2; return 3; } else { return 4; } }'

assert 10 'int main() { int i = 10; return i; }'
assert 55 'int main() { int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }'
assert 3 'int main() { for (;;) return 3; return 5; }'
assert 3 'int main() { for (;;) { return 3; } return 5; }'
assert 10 'int main() { int i=0; for (;i<10;) i=i+1; return i; }'
assert 3 'int main() { { 1; { 2; } return 3; } }'

assert 10 'int main() { int i=0; while(i<10) i=i+1; return i; }'
assert 10 'int main() { int i=0; while(i<10) { i=i+1; } return i; }'
assert 55 'int main() { int i=0; int j=0; while(i<=10) { j=i+j; i=i+1; } return j; }'
assert 55 'int main() { int a=0; int b=1; int i=0; while(i<10) { int t=a; a=b; b=t+b; i=i+1; } return a; }'

assert 3 'int main() { int x=3; return *&x; }'
assert 3 'int main() { int x=3; int* y=&x; int** z=&y; return **z; }'
assert 5 'int main() { int x=3; int* y=&x; *y=5; return x; }'

assert 5 'int main() { int x=3; int y=5; return *(&x-1); }' # ub
assert 3 'int main() { int x=3; int y=5; return *(&y+1); }' # ub
assert 5 'int main() { int x=3; int y=5; return *(&x+(-1)); }' # ub

assert 7 'int main() { int x=3; int y=5; *(&x-1)=7; return y; }' # ub
assert 7 'int main() { int x=3; int y=5; *(&y+1)=7; return x; }' # ub
assert 7 'int main() { int x=3; int y=5; *(&y+2-1)=7; return x; }' # ub

assert 5 'int main() { int x=3; return (&x+2)-&x+3; }'
assert 8 'int main() { int x, y; x=3; y=5; return x+y; }'
assert 8 'int main() { int x=3, y=5; return x+y; }'
assert 3 'int main() { int x=3, *y=&x; return *y; }'

assert 3 'int main() { return ret3(); }'
assert 5 'int main() { return ret5(); }'

assert 8 'int main() { return add(3, 5); }'
assert 2 'int main() { return sub(5, 3); }'
assert 21 'int main() { return add6(1,2,3,4,5,6); }'
assert 66 'int main() { return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }'
assert 136 'int main() { return add6(1,2,add6(3,add6(4,5,6,7,8,9),10,11,12,13),14,15,16); }'

# assert 0 'int main() { 1 = 1; return 0; }'

echo OK
