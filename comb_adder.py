import numpy as np
from bitarray import bitarray
import random

def test_A():
    rnd = random.Random()

    #n = 2**14
    n = 2**3

    xn = 5
    yn = 3

    zero = bitarray(n)
    zero.setall(0)
    print(zero)

    x = [ rnd.randrange(1<<xn) for _ in range(n)]
    y = [ rnd.randrange(1<<yn) for _ in range(n)]

    xs = []
    for j in range(xn):
        xs.append( bitarray( b & (1<<j) for b in x))

    ys = []
    for j in range(yn):
        ys.append( bitarray( b & (1<<j) for b in y))

    result = []

    cs = [zero for _ in range(yn)]
    for i in range(xn+yn):
        s = zero
        print(s)
        for j in range(yn):
            a = s
            b = (xs[i-j] if (0 <= i-j < xn) else zero) & ys[j]
            c = cs[j]
            s = a^b^c
            co = a&b|a&c|b&c 
            print(a,b,c,s,co)
            cs[j] = co
        print(s) 
        result.append(s)

    print(result)

    np_sums = np.zeros( n, np.int64)
    for i in range(xn+yn):
      np_sums += (1<<i)*np.array(list(result[i]))

    sums = []
    for idx in range(n):
      s = 0
      for i in range(xn+yn):
         s += (1<<i) if result[i][idx] else 0
      sums.append(s)

    print(x)
    print(y)

    expected = np.array( [u*v for u,v in zip(x,y)], dtype=np.int64)
    actual = np.array( sums, dtype=np.int64)

    print( expected)
    print( actual)
    print( np_sums)
    assert np.allclose( expected, actual)


