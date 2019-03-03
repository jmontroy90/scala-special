package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal { Math.pow(b(), 2) - (4 * a() * c()) }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a, b, c)()
      if (delta < 0) Set.empty[Double]
      else {
        val bSnap = b()
        val aSnap = a()
        Set(
          (-1 * bSnap + Math.sqrt(delta)) / (2 * aSnap),
          (-1 * bSnap - Math.sqrt(delta)) / (2 * aSnap)
        )
      }
    }
  }
}
