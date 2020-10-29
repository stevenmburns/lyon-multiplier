import numpy as np
from bitarray import bitarray
import random

def test_A():
    rnd = random.Random()

    n = 2**14

    xn = 16
    yn = 16

    zero = bitarray(n)
    zero.setall(0)

    x = np.array([ rnd.randrange(1<<xn) for _ in range(n)], dtype=np.int64)
    y = np.array([ rnd.randrange(1<<yn) for _ in range(n)], dtype=np.int64)

    xs = []
    for j in range(xn):
        xs.append( bitarray( b & (1<<j) for b in x))

    ys = []
    for j in range(yn):
        ys.append( bitarray( b & (1<<j) for b in y))

    result = []

    ops = 0
    cs = [zero for _ in range(yn)]
    for i in range(xn+yn):
        s = zero
        for j in range(yn):
            a = s
            b = (xs[i-j] if (0 <= i-j < xn) else zero) & ys[j]
            ops += 1
            c = cs[j]
            s = a^b^c
            ops += 2
            co = a&b|a&c|b&c 
            ops += 5
            cs[j] = co
        result.append(s)

    print(f"Ops: {ops} loops: {(xn+yn)*yn}")

    actual = np.zeros( n, np.int64)
    for i in range(xn+yn):
      actual += (1<<i)*np.array(list(result[i]))

    print(x)
    print(y)

    expected = x*y

    print( expected)
    print( actual)
    assert np.allclose( expected, actual)


