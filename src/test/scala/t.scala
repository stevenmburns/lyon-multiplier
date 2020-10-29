
package lyon

//import chisel3._
//import chisel3.util._
import chisel3.iotesters._

class NullTester[T <: BitcellIfc]( factory: () => T) extends GenericTest {
  behavior of s"DUT on null input"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        poke( c.l.x, 0)
        poke( c.l.y, 0)
        poke( c.l.pp, 0)
        poke( c.l.r, 0)
        step(1)
        expect( c.r.x, 0)
        expect( c.r.y, 0)
        expect( c.r.pp, 0)
        expect( c.r.r, 0)
      }
    } should be (true)
  }
}

class NullArrayTester[T <: BitcellIfc]( factory: () => BitcellArray[T]) extends GenericTest {
  behavior of s"BitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        println( s"Number of stages: ${c.k}")
        poke( c.l.x, 0)
        poke( c.l.y, 0)
        poke( c.l.pp, 0)
        poke( c.l.r, 0)
        step(c.k)
        expect( c.r.x, 0)
        expect( c.r.y, 0)
        expect( c.r.pp, 0)
        expect( c.r.r, 0)
      }
    } should be (true)
  }
}

class NullSimpleArrayTester[T <: SimpleBitcellIfc]( factory: () => SimpleBitcellArray[T]) extends GenericTest {
  behavior of s"SimpleBitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        println( s"Number of stages: ${c.k}")
        for { i <- 0 until c.k} {
          poke( c.yin(i), 0)
          poke( c.rin(i), 0)
        }
        poke( c.l.x, 0)
        poke( c.l.pp, 0)
        step(1)
        expect( c.r.x, 0)
        expect( c.r.pp, 0)
      }
    }
  }
}

class SimpleArrayTester[T <: SimpleBitcellIfc]( factory: () => SimpleBitcellArray[T], val n : Int = 8, val optYvalue : Option[BigInt] = None) extends GenericTest {
  behavior of s"SimpleBitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {

        require( c.k <= n)

        // x is less than 2^(n-1) then there will be no overflow
	val xvalues = IndexedSeq(29,0)
	// val xvalues = for { _ <- 0 until 100} yield BigInt(n-1,rnd)
	//val xvalues = for { _ <- 0 until 100} yield BigInt(n,rnd)

	val mask_n = (BigInt(1)<<n)-1

        // Halve by default
        val yvalue = optYvalue.getOrElse( BigInt(1) << (c.k-2))
        //val zvalues = for { xvalue <- xvalues} yield ((yvalue*xvalue)>>(c.k-1))
        val zvalues = for { xvalue <- xvalues} yield ((yvalue*xvalue)>>(c.k-1)) & mask_n

        println( s"$zvalues")

        def lsb_first( n : Int, x : BigInt) : List[BigInt] =
          if ( n == 0) {
            List()
          } else {
            (x % 2) :: lsb_first( n-1, x>>1)
          }

        def from_lsb_first( s : IndexedSeq[BigInt]) : BigInt =
          s.reverse.foldLeft(BigInt(0)){ (x, el) => (x<<1) | el}

        val x_lsb_first_array = for { xvalue <- xvalues} yield lsb_first( n, xvalue).toIndexedSeq

        val x_lsb_first = for { s <- x_lsb_first_array; x <- s} yield x
        println( s"x_lsb: ${x_lsb_first}")

        val y_lsb_first = lsb_first( c.k, yvalue).toIndexedSeq
        println( s"y_lsb: ${y_lsb_first}")

        println( s"Number of bits in x: ${n}")
        println( s"Number of stages (bits in y): ${c.k}")
        for { j <- 0 until c.k} {
          poke( c.yin(j), if ( j< y_lsb_first.size) y_lsb_first(j) else BigInt(0))
        }

        poke( c.l.pp, 0)

        val z_lsb_first = 
          for { i <- 0 until x_lsb_first.size+c.k-1} yield {

            for { j <- 0 until c.k} {
              poke( c.rin(j), if (((i-j) % n) == n-1) 0 else 1)
            }

            poke( c.l.x, if ( i< x_lsb_first.size) x_lsb_first(i) else BigInt(0))

            val pp = peek( c.r.pp)
            step(1)
            pp
          }

        println( s"$z_lsb_first")
        println( s"${z_lsb_first.drop(c.k-1).sliding(n,n).toIndexedSeq}")

        val zs = (for { s <- z_lsb_first.drop(c.k-1).sliding(n,n)} yield from_lsb_first(s)).toIndexedSeq

        for { (x,expected,actual) <- (xvalues,zvalues,zs).zipped} {
          expect( expected == actual, s"x: ${x} y: ${yvalue} Expected: ${expected} Actual: ${actual}")
        }
      }
    } should be (true)
  }
}

class TableauTester( factory: () => Tableau) extends GenericTest {
  behavior of s"TableauTester"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {

        // x is less than 2^(n-1) then there will be no overflow
	//val xvalues = IndexedSeq(31,0)
	val xvalues = for { _ <- 0 until c.m} yield BigInt(c.n-1,rnd)

	require( c.k == 3)

	val mask_n = (BigInt(1)<<c.n)-1

        val yvalue = 7
        //val zvalues = for { xvalue <- xvalues} yield ((yvalue*xvalue)>>(c.k-1))
        val zvalues = for { xvalue <- xvalues} yield ((yvalue*xvalue)>>(c.k-1)) & mask_n

        println( s"$zvalues")

        def lsb_first( n : Int, x : BigInt) : List[BigInt] =
          if ( n == 0) {
            List()
          } else {
            (x % 2) :: lsb_first( n-1, x>>1)
          }

        def from_lsb_first( s : IndexedSeq[BigInt]) : BigInt =
          s.reverse.foldLeft(BigInt(0)){ (x, el) => (x<<1) | el}

        val x_lsb_first_array = for { xvalue <- xvalues} yield lsb_first( c.n, xvalue).toIndexedSeq
        val x_lsb_first = for { s <- x_lsb_first_array; x <- s} yield x

        println( s"x_lsb: ${x_lsb_first}")

        val y_lsb_first = lsb_first( c.k, yvalue).toIndexedSeq
        println( s"y_lsb: ${y_lsb_first}")

        println( s"Number of bits in x: ${c.n}")
        println( s"Number of stages (bits in y): ${c.k}")

	for { i <- 0 until c.n*c.m} {
	  poke( c.x(i), x_lsb_first(i))
	}
	for { j <- 0 until c.k} {
	  poke( c.y(j), y_lsb_first(j))
	}

	step(1)

        val z_lsb_first = 
          for { i <- 0 until c.m*c.n+c.k} yield {
	    peek( c.s_out(i))
          }

        println( s"$z_lsb_first")
        println( s"${z_lsb_first.drop(c.k-1).sliding(c.n,c.n).toIndexedSeq}")

        val zs = (for { s <- z_lsb_first.drop(c.k-1).sliding(c.n,c.n)} yield from_lsb_first(s)).toIndexedSeq

        for { (x,expected,actual) <- (xvalues,zvalues,zs).zipped} {
          expect( expected == actual, s"x: ${x} y: ${yvalue} Expected: ${expected} Actual: ${actual}")
        }
      }
    } should be (true)
  }
}

class TableauTest extends TableauTester( () => new Tableau( m=100, n=5, k=3))

class TestOptionDelay extends NullTester( () => new OptionalDelay)
class TestPipelinedBitcell extends NullTester( () => new PipelinedBitcell)

class NullSimpleArrayTestBitcell     extends NullSimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell))
class NullArrayTestPipelinedBitcell  extends NullArrayTester( () => new BitcellArray( 8, () => new PipelinedBitcell))

class SimpleArrayTestBitcell_3_5_7   extends SimpleArrayTester( () => new SimpleBitcellArray( 3, () => new SimpleBitcell), 5, Some(7))

class SimpleArrayTestBitcell_3_8_4   extends SimpleArrayTester( () => new SimpleBitcellArray( 3, () => new SimpleBitcell), 8, None)
class SimpleArrayTestBitcell_3_8_2   extends SimpleArrayTester( () => new SimpleBitcellArray( 3, () => new SimpleBitcell), 8, Some(2))

class SimpleArrayTestBitcell_8_8_64  extends SimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell), 8, Some(64))
class SimpleArrayTestBitcell_8_8_128 extends SimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell), 8, None)

class SimpleArrayTestBitcell_3_8_7   extends SimpleArrayTester( () => new SimpleBitcellArray( 3, () => new SimpleBitcell), 8, Some(7))
class SimpleArrayTestBitcell_4_8_15  extends SimpleArrayTester( () => new SimpleBitcellArray( 4, () => new SimpleBitcell), 8, Some(15))
class SimpleArrayTestBitcell_5_8_31  extends SimpleArrayTester( () => new SimpleBitcellArray( 5, () => new SimpleBitcell), 8, Some(31))
class SimpleArrayTestBitcell_6_8_63  extends SimpleArrayTester( () => new SimpleBitcellArray( 6, () => new SimpleBitcell), 8, Some(63))
class SimpleArrayTestBitcell_7_8_127 extends SimpleArrayTester( () => new SimpleBitcellArray( 7, () => new SimpleBitcell), 8, Some(127))
class SimpleArrayTestBitcell_8_8_255 extends SimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell), 8, Some(255))
