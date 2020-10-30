import numpy as np
from bitarray import bitarray
import random

def random_array( rnd, n, nbits):
    assert 1 <= nbits <= 64
    return np.array([ rnd.randrange(1<<nbits) for _ in range(n)], dtype=np.int64)
    
def convert_to_bits_lsb_first( xn, x):
    return [ bitarray( b & (1<<j) for b in x) for j in range(xn)]

def convert_from_bits_lsb_first( zs):
    actual = np.zeros( len(zs[0]), np.int64)
    for i,z in enumerate(zs):
      actual += (1<<i)*np.array(list(z))
    return actual

def carry_save( a, b, c):
    s = a^b^c
    c = a&b|a&c|b&c 
    return s,c

def add(xs,ys):
    n = len(xs[0])
    xn = len(xs)
    yn = len(ys)
    zn = max(xn,yn)+1
    ops = zn*(2+5)
    print(f"Ops: {ops} loops: {zn}")

    zero = bitarray(n)
    zero.setall(0)

    zs = []

    c = zero
    for i in range(zn):
        a = xs[i] if (i < xn) else zero
        b = ys[i] if (i < yn) else zero
        s,c = carry_save(a,b,c)
        zs.append(s)

    return zs

def mult(xs,ys):
    n = len(xs[0])
    xn = len(xs)
    yn = len(ys)
    zn = xn+yn
    ops = zn*yn*(1+2+5)
    print(f"Ops: {ops} loops: {zn*yn}")

    zero = bitarray(n)
    zero.setall(0)

    zs = []
    cs = [zero for _ in range(yn)]
    for i in range(zn):
        s = zero
        for j in range(yn):
            a = s
            b = (xs[i-j] if (0 <= i-j < xn) else zero) & ys[j]
            s, cs[j] = carry_save(a,b,cs[j])
        zs.append(s)

    return zs


def test_add():
    rnd = random.Random()

    n = 2**14

    xn = 5
    yn = 1

    x = random_array( rnd, n, xn)
    y = random_array( rnd, n, yn)

    xs = convert_to_bits_lsb_first( xn, x)
    ys = convert_to_bits_lsb_first( yn, y)

    result = add(xs,ys)

    actual = convert_from_bits_lsb_first( result)

    print(x)
    print(y)

    expected = x+y

    print( expected)
    print( actual)
    assert np.allclose( expected, actual)

def test_mult():
    rnd = random.Random()

    n = 2**14

    xn = 5
    yn = 3

    x = random_array( rnd, n, xn)
    y = random_array( rnd, n, yn)

    xs = convert_to_bits_lsb_first( xn, x)
    ys = convert_to_bits_lsb_first( yn, y)

    result = mult(xs,ys)

    actual = convert_from_bits_lsb_first( result)

    print(x)
    print(y)

    expected = x*y

    print( expected)
    print( actual)
    assert np.allclose( expected, actual)


