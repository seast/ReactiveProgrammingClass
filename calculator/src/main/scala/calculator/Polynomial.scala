package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b.apply * b.apply - 4 * a.apply * c.apply
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val delta = Math.sqrt(computeDelta(a,b,c).apply)
      Set((-1 * b.apply + delta) / (2 * a.apply), 
          (-1 * b.apply - delta) / (2 * a.apply)) 
    }
  }
}
