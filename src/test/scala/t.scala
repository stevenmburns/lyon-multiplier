
package lyon

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}

import chisel3._
import chisel3.util._
import chisel3.iotesters._

class FourWire extends Bundle {
  val x = Output(Bool())
  val y = Output(Bool())
  val pp = Output(Bool())
  val r = Output(Bool())
}

class BitcellIfc extends MultiIOModule {
  val l = IO(Flipped(new FourWire))
  val r = IO(new FourWire)
}


class OptionalDelay extends BitcellIfc {
  r.x := RegNext( l.x)
  r.y := RegNext( l.y)
  r.pp := RegNext( l.pp)
  r.r := RegNext( l.r)
}


object Sum {
  def apply( a : Bool, b : Bool, ci : Bool) : (Bool, Bool) = {
    val s = a ^ b ^ ci
    val co = a&b | a&ci | b&ci
    ( s, co)
  }
}

class Bitcell extends BitcellIfc {
  r.x := RegNext(l.x)
  r.y := l.y

  val coeff = l.y
  val carry = Reg(Bool())

  val a = WireInit( init=r.x & coeff)

  val (s,co) = Sum( a, l.pp, carry)
  carry := co

  r.pp := s & r.r

  r.r := l.r
}

class PipelinedBitcell extends BitcellIfc {
  r.x := RegNext(l.x)
  r.y := RegNext(l.y)

  val coeff = Reg(Bool())
  val carry = Reg(Bool())

  coeff := Mux( l.r, coeff, l.y)

  val a = WireInit( init=r.x & coeff)

  val (s,co) = Sum( a, l.pp, carry)
  carry := co

  r.pp := s & r.r

  r.r := RegNext(l.r)
}

class BitcellArray[T <: BitcellIfc]( val n : Int, factory: () => T) extends BitcellIfc {
  r := (IndexedSeq.fill(n){ Module(factory())}).foldLeft( l){ (left,m) =>
    m.l := left
    m.r
  }
}

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

class TestOptionDelay extends NullTester( () => new OptionalDelay)
class TestBitcell extends NullTester( () => new Bitcell)
class TestPipelinedBitcell extends NullTester( () => new PipelinedBitcell)

class NullArrayTestBitcell          extends NullArrayTester( () => new BitcellArray( 8, () => new Bitcell))
class NullArrayTestPipelinedBitcell extends NullArrayTester( () => new BitcellArray( 8, () => new PipelinedBitcell))
