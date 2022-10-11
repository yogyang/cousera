package example


@main def run(): Unit = {
//  val tests = Seq(0.001, 0.1e-20, 1.0e20, 1.0e50)
  val tests = Seq(1.0e50)
  tests.foreach(x => println(sqrt(x)))
}


def abs(x:Double) = if (x < 0) -x else x

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) : Boolean = {
  abs(guess * guess - x) / x < 0.0000001
}

def improve(guess: Double, x: Double) =(guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1.0, x)
