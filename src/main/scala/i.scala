package lyon

import chisel3._
import chisel3.util._

class FourWire extends Bundle {
  val x = Output(Bool())
  val y = Output(Bool())
  val pp = Output(Bool())
  val r = Output(Bool())
}

class TwoWire extends Bundle {
  val x = Output(Bool())
  val pp = Output(Bool())
}

class SimpleBitcellIfc extends MultiIOModule {
  val l = IO(Flipped(new TwoWire))
  val r = IO(new TwoWire)

  val yin = IO(Input(Bool()))
  val rin = IO(Input(Bool()))
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

class SimpleBitcell extends SimpleBitcellIfc {

  r.x := RegNext(l.x)

  val coeff = yin
  val carry = Reg(Bool())

  val a = WireInit( init=r.x & coeff)

  val (s,co) = Sum( a, l.pp, carry)
  carry := co

  r.pp := s & rin
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

class SimpleBitcellArray[T <: SimpleBitcellIfc]( val n : Int, factory: () => T) extends MultiIOModule {
  val l = IO(Flipped(new TwoWire))
  val r = IO(new TwoWire)

  val yin = IO(Input(Vec(n,Bool())))
  val rin = IO(Input(Vec(n,Bool())))

  val ms = IndexedSeq.fill(n){ Module(factory())}
  r := (yin,rin,ms).zipped.foldLeft( l){ case (left,(yy,rr,m)) =>
    m.l := left
    m.yin := yy
    m.rin := rr
    m.r
  }
}

class BitcellArray[T <: BitcellIfc]( val n : Int, factory: () => T) extends BitcellIfc {
  r := (IndexedSeq.fill(n){ Module(factory())}).foldLeft( l){ (left,m) =>
    m.l := left
    m.r
  }
}
