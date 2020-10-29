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

def add(xs,ys):
    n = len(xs[0])
    xn = len(xs)
    yn = len(ys)
 
    zero = bitarray(n)
    zero.setall(0)

    result = []

    c = zero
    for i in range(max(xn,yn)+1):
        a = xs[i] if (i < xn) else zero
        b = ys[i] if (i < yn) else zero
        s = a^b^c
        c = a&b|a&c|b&c 
        result.append(s)

    ops = (max(xn,yn)+1)*(2+5)

    print(f"Ops: {ops} loops: {max(xn,yn)+1}")
    return result

def mult(xs,ys):
    n = len(xs[0])
    xn = len(xs)
    yn = len(ys)

    zero = bitarray(n)
    zero.setall(0)

    result = []
    cs = [zero for _ in range(yn)]
    for i in range(xn+yn):
        s = zero
        for j in range(yn):
            a = s
            b = (xs[i-j] if (0 <= i-j < xn) else zero) & ys[j]
            c = cs[j]
            s = a^b^c
            cs[j] = a&b|a&c|b&c 
        result.append(s)

    ops = (xn+yn)*yn*(1+2+5)
    print(f"Ops: {ops} loops: {(xn+yn)*yn}")

    return result


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


