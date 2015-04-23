package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val root = (sing: Int) => (-b() + sing * Math.sqrt(delta())) / (2 * a())
    Signal[Set[Double]](if (delta() < 0) Set() else Set(root(1), root(-1)))
  }
}
