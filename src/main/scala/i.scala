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

  val j = IO(Input(UInt(8.W)))
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

  r.x := RegNext(l.x,init=false.B)

  val coeff = yin
  val carry = RegInit(init=false.B)

  val a = WireInit( init=l.x & coeff)

  val (s,co) = Sum( a, l.pp, carry)
  carry := co

  r.pp := Mux(rin, s, carry)

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

class SimpleBitcellArray[T <: SimpleBitcellIfc]( val k : Int, factory: () => T) extends MultiIOModule {
  val l = IO(Flipped(new TwoWire))
  val r = IO(new TwoWire)

  val yin = IO(Input(Vec(k,Bool())))
  val rin = IO(Input(Vec(k,Bool())))

  val ms = IndexedSeq.fill(k){ Module(factory())}

  val z = yin zip rin zip ms zipWithIndex

  r := z.foldLeft( l){ case (left,(((yy,rr),m),idx)) =>
    m.l := left
    m.yin := yy
    m.rin := rr
    m.j := idx.U    
    m.r
  }
}

class BitcellArray[T <: BitcellIfc]( val k : Int, factory: () => T) extends BitcellIfc {
  r := (IndexedSeq.fill(k){ Module(factory())}).foldLeft( l){ (left,m) =>
    m.l := left
    m.r
  }
}

class Tableau( val m : Int, val n : Int, val k : Int) extends MultiIOModule {

  val x = IO(Input(Vec(m*n,Bool())))
  val y = IO(Input(Vec(k,Bool())))

  val s_out = IO(Output(Vec(m*n+k,Bool())))

  val csas = for { j <- 0 until k} yield {
    for { i <- 0 until m*n+k} yield {
      val a = Wire(Bool())
      val b = Wire(Bool())
      val c = Wire(Bool())
      val (s,co) = Sum( a, b, c)            
      (a,b,c,s,co)
    } 
  }
  for { j <- 0 until k} {
    for { i <- 0 until m*n+k} {
      csas(j)(i)._2 := (if ( 0<= i-j && i-j < m*n) x(i-j) else false.B) && y(j)
    }
  }

  for { j <- 0 until k} {
    csas(j)(0)._3 := false.B
    for { i <- 0 until m*n+k-1} {
       csas(j)(i+1)._3 := csas(j)(i)._5
    }  
  }
  for { i <- 0 until m*n+k} {
    csas(0)(i)._1 := false.B
  }  
  for { j <- 0 until k-1} {
    for { i <- 0 until m*n+k} {
       csas(j+1)(i)._1 := csas(j)(i)._4
    }  
  }
  for { i <- 0 until m*n+k} {
     s_out(i) := csas(k-1)(i)._4
     if ( i % n == k-2) {
       s_out(i) := csas(k-1)(i-1)._5
     }
  }  

  for { j <- 1 until k} {
    for { ii <- 0 until m} {
      val i = ii*n + n
      val (_,_,_,_,co) = csas(j-1)(i+j-1)
      val (a,_,_,_,_)  = csas(j)(i+j-1)
      a := co
    }       
  }
  for { j <- 0 until k} {
    for { ii <- 0 until m} {
      val i = ii*n + n
      val (_,_,c,_,_) = csas(j)(i+j)
      c := false.B
    }       
  }

/*
  printf( "as:\n")
  for { j <- 0 until k} {
    for { i <- m*n+k-1 to 0 by -1} {
      printf( "%b", csas(j)(i)._1)	
    }
    printf( "\n")
  }
  printf( "bs:\n")
  for { j <- 0 until k} {
    for { i <- m*n+k-1 to 0 by -1} {
      printf( "%b", csas(j)(i)._2)	
    }
    printf( "\n")
  }
  printf( "cs:\n")
  for { j <- 0 until k} {
    for { i <- m*n+k-1 to 0 by -1} {
      printf( "%b", csas(j)(i)._3)	
    }
    printf( "\n")
  }
  printf( "Sums:\n")
  for { j <- 0 until k} {
    for { i <- m*n+k-1 to 0 by -1} {
      printf( "%b", csas(j)(i)._4)	
    }
    printf( "\n")
  }
  printf( "Carries:\n")
  for { j <- 0 until k} {
    for { i <- m*n+k-1 to 0 by -1} {
      printf( "%b", csas(j)(i)._5)	
    }
    printf( "\n")
  }
*/

}