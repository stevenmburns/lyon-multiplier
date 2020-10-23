
package lyon

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}

import chisel3._
import chisel3.util._
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
    }
  }
}

class NullArrayTester[T <: BitcellIfc]( factory: () => BitcellArray[T]) extends GenericTest {
  behavior of s"BitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        println( s"Number of stages: ${c.n}")
        poke( c.l.x, 0)
        poke( c.l.y, 0)
        poke( c.l.pp, 0)
        poke( c.l.r, 0)
        step(c.n)
        expect( c.r.x, 0)
        expect( c.r.y, 0)
        expect( c.r.pp, 0)
        expect( c.r.r, 0)
      }
    }
  }
}

class NullSimpleArrayTester[T <: SimpleBitcellIfc]( factory: () => SimpleBitcellArray[T]) extends GenericTest {
  behavior of s"SimpleBitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        println( s"Number of stages: ${c.n}")
        for { i <- 0 until c.n} {
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

class SimpleArrayTester[T <: SimpleBitcellIfc]( factory: () => SimpleBitcellArray[T]) extends GenericTest {
  behavior of s"SimpleBitcellArray"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        val xvalue = 45
        val yvalue = 17

        def lsb_first( x : BigInt) : List[BigInt] =
          if ( x == 0) {
            List()
          } else {
            (x % 2) :: lsb_first( x>>1)
          }

        def from_lsb_first( s : IndexedSeq[BigInt]) : BigInt =
          s.reverse.foldLeft(BigInt(0)){ (x, el) => (x<<1) | el}

        val x_lsb_first = lsb_first(xvalue).toIndexedSeq
        println( s"x_lsb: ${x_lsb_first}")
        val y_lsb_first = lsb_first(yvalue).toIndexedSeq
        println( s"y_lsb: ${y_lsb_first}")

        println( s"Number of stages: ${c.n}")
        for { i <- 0 until c.n} {
          poke( c.yin(i), if ( i< y_lsb_first.size) y_lsb_first(i) else BigInt(0))
          poke( c.rin(i), 1)
        }
        poke( c.l.pp, 0)
        val z_lsb_first = 
          for { i <- 0 until 2*c.n} yield {
            poke( c.l.x, if ( i< x_lsb_first.size) x_lsb_first(i) else BigInt(0))
            step(1)
            val pp = peek( c.r.pp)
            pp
          }

        val z = from_lsb_first(z_lsb_first)

        expect( z == xvalue*yvalue, s"Correct value: $z ${xvalue*yvalue}")
        println( s"z: ${z} ${z.toString(2)}")
      }
    }
  }
}

class TestOptionDelay extends NullTester( () => new OptionalDelay)
class TestPipelinedBitcell extends NullTester( () => new PipelinedBitcell)

class NullSimpleArrayTestBitcell extends NullSimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell))
class NullArrayTestPipelinedBitcell extends NullArrayTester( () => new BitcellArray( 8, () => new PipelinedBitcell))

class SimpleArrayTestBitcell extends SimpleArrayTester( () => new SimpleBitcellArray( 8, () => new SimpleBitcell))
