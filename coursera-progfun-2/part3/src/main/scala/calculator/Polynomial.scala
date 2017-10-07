package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal { Math.pow(b(), 2) - 4*a()*c()}
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Signal {
      val currentDelta = delta()
      if(currentDelta < 0)
        Set.empty
      else
        Set(
          (-b() + Math.sqrt(currentDelta))/(2*a()),
          (-b() - Math.sqrt(currentDelta))/(2*a())
        )
    }
  }
}
