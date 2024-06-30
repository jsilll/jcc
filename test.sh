#!/bin/bash

assert() {
    expected="$1"
    input="$2"

    echo "Compiling $input"
    ./bin/jcc "$input" --emit-tokens --emit-ast > tmp.s || exit
    gcc -static -o tmp tmp.s

    ./tmp
    actual="$?"
    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

make || exit

assert 0 '{ return 0; }'
assert 42 '{ return 42; }'
assert 21 '{ return 5+20-4; }'
assert 41 '{ return  12 + 34 - 5 ; }'
assert 47 '{ return 5+6*7; }'
assert 15 '{ return 5*(9-6); }'
assert 4 '{ return (3+5)/2; }'
assert 10 '{ return -10+20; }'
assert 10 '{ return - -10; }'
assert 10 '{ return - - +10; }'

assert 0 '{ return 0==1; }'
assert 1 '{ return 42==42; }'
assert 1 '{ return 0!=1; }'
assert 0 '{ return 42!=42; }'

assert 1 '{ return 0<1; }'
assert 0 '{ return 1<1; }'
assert 0 '{ return 2<1; }'
assert 1 '{ return 0<=1; }'
assert 1 '{ return 1<=1; }'
assert 0 '{ return 2<=1; }'

assert 1 '{ return 1>0; }'
assert 0 '{ return 1>1; }'
assert 0 '{ return 1>2; }'
assert 1 '{ return 1>=0; }'
assert 1 '{ return 1>=1; }'
assert 0 '{ return 1>=2; }'

assert 3 '{ int a = 0; a=3; return a; }'
assert 8 '{ int a; int z; a=3; z=5; return a+z; }'

assert 3 '{ int a; a=3; return a; }'
assert 8 '{ int a; int z; a=3; z=5; return a+z; }'
assert 6 '{ int a; int b; a=b=3; return a+b; }'
assert 3 '{ int foo; foo=3; return foo; }'
assert 8 '{ int foo123; int bar; foo123=3; bar=5; return foo123+bar; }'
assert 6 '{ int _a_123; int _b_456; _a_123=_b_456=3; return _a_123+_b_456; }'

assert 1 '{ return 1; 2; 3; }'
assert 2 '{ 1; return 2; 3; }'
assert 3 '{ 1; 2; return 3; }'

assert 3 '{ {1; {2;} return 3;} }'
assert 5 '{ ;;; return 5; }'
assert 1 '{ return 1; return 2; }'

assert 3 '{ if (0) return 2; return 3; }'
assert 3 '{ if (1-1) return 2; return 3; }'
assert 2 '{ if (1) return 2; return 3; }'
assert 2 '{ if (2-1) return 2; return 3; }'
assert 4 '{ if (0) { 1; 2; return 3; } else { return 4; } }'
assert 3 '{ if (1) { 1; 2; return 3; } else { return 4; } }'

assert 10 '{ int i = 10; return i; }'
assert 55 '{ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }'
assert 3 '{ for (;;) {return 3;} return 5; }'
assert 10 '{ int i=0; for (;i<10;) i=i+1; return i; }'

assert 10 '{ int i=0; while(i<10) { i=i+1; } return i; }'
assert 55 '{ int i=0; int j=0; while(i<=10) { j=i+j; i=i+1; } return j; }'
assert 55 '{ int a=0; int b=1; int i=0; while(i<10) { int t=a; a=b; b=t+b; i=i+1; } return a; }'

assert 3 '{ int x=3; return *&x; }'
assert 5 '{ int x=3; int* y=&x; *y=5; return x; }'
assert 3 '{ int x=3; int* y=&x; int** z=&y; return **z; }'

assert 5 '{ int x=3; int y=5; return *(&x-1); }' # ub
assert 3 '{ int x=3; int y=5; return *(&y+1); }' # ub
assert 5 '{ int x=3; int y=5; return *(&x+(-1)); }' # ub
assert 7 '{ int x=3; int y=5; *(&x-1)=7; return y; }' # ub
assert 7 '{ int x=3; int y=5; *(&y+1)=7; return x; }' # ub
assert 7 '{ int x=3; int y=5; *(&y+2-1)=7; return x; }' # ub

# assert 0 '{ 1 = 1; return 0; }'

