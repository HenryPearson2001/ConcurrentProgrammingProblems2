import io.threadcso._
import scala.collection.mutable.Queue

class Adaptive(f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
    require(a <= b)

    // total to store the current total of the integrals
    private var total: Double = 0.0

    // each task is one interval to calculate
    private type Task = (Double, Double)

    // communication channels
    private val toWorkers = OneMany[Task]
    private val toController = OneMany[Task]
    private val toTotal = OneMany[Double]

    // queue to store the tasks - initially with the entire integral
    private val tasks = Queue[Task]((a,b))

    // calculate one trapezium for f between left and right
    private def trapezium(left: Double, right: Double): Double = {
        val average = (f(left) + f(right)) / 2
        average * (right-left)
    }

    // single worker
    private def worker = proc {
        repeat {
            val (i, j) = toWorkers?()
            val mid = (i + j) / 2.0
            val trap1 = trapezium(i, mid)
            val trap2 = trapezium(mid, j)
            val trap3 = trapezium(i, j)
            if ((trap1 + trap2 - trap3).abs < Epsilon) {
                toTotal!(trap3)
            }
            else {
                toController!((i, mid))
                toController!((mid, j))
            }
        }
    }

    private def distributor = proc {
        var activeTasks = 0
        serve (
            (!tasks.isEmpty && toWorkers) =!=> { activeTasks = activeTasks + 2; val x = tasks.dequeue; x }
            | (toController)  =?=> { v => tasks.enqueue(v); activeTasks = activeTasks - 1 }
            | (toTotal) =?=> {
                v =>
                    activeTasks = activeTasks - 2
                    total = total + v
                    if (activeTasks == 0 && tasks.isEmpty) { toWorkers.close; toController.close; toTotal.close }
            }
        )
    }

    def apply(): Double = {
        val workers = || (for (i <- 0 until nWorkers) yield worker)
        run (workers || distributor)
        total
    }
}

import scala.util.Random

import io.threadcso._

/** Object to test the concurrent Trapezium rule code. */
object TrapeziumTest{
  /** We will test the Trapezium class by selecting random polynomials.  Each
    * polynomial will be represented by an array of its coefficients.  The
    * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
    * indices. */
  type Polynomial = Array[Double]

  /* We'll create random polynomials, with degree uniform in [0..MaxDegree), and
    * coefficients uniform in [-MaxCoeff..MaxCoeff). */
  val random = scala.util.Random
  val MaxDegree = 5
  val MaxCoeff = 100

  /** Random Double in [-max, max). */
  def uniform(max: Double): Double = max*(2*random.nextDouble-1)

  /** Create a random polynomial. */
  def mkPoly: Polynomial =
    Array.fill(1+random.nextInt(MaxDegree))(uniform(MaxCoeff))

  /** Convert a poly to a string. */
  def toString(poly: Polynomial): String =
    (0 until poly.length).map(i => poly(i).toString+"x^"+i).mkString(" + ")

  /** Evaluate poly at x. */
  def evalPoly(poly: Polynomial)(x: Double): Double = {
    // Use Horner's rule
    var result = 0.0
    for(i <- poly.size-1 to 0 by -1) result = result*x + poly(i)
    result
  }

}

/** A test for adaptive quadrature.
  *
  * Note: this assumes that TrapeziumTest and Adaptive are on the current
  * path. */
object AdaptiveTest{
  val Epsilon = 1E-6

  /** We will test the Adaptive class by selecting random polynomials.  Each
    * polynomial will be represented by an array of its coefficients.  The
    * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
    * indices. */
  type Polynomial = Array[Double]

  /** Estimate the integrap of f from a to b using adaptive quadrature. */
  def estimate(f: Double => Double, a: Double, b: Double) : Double = {
    val mid = (a+b)/2.0
    val fa = f(a); val fb = f(b); val fmid = f(mid)
    val lArea = (fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
    val area = (fa+fb)*(b-a)/2
    if (Math.abs(lArea+rArea-area) < Epsilon) area
    else estimate(f,a,mid) + estimate(f,mid,b)
  }

  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
    * from a to b should be estimated using nWorkers workers, and that f
    * corresponds to p. */
  def pickParams: (Double => Double, Polynomial, Double, Double, Int) = {
    // function to evaluate
    val p = TrapeziumTest.mkPoly; val f = TrapeziumTest.evalPoly(p)(_)
    // limits
    val a = TrapeziumTest.uniform(10); val b = a+10*Random.nextDouble
    // Number of workers
    val nWorkers = 1+Random.nextInt(16)
    (f, p, a, b, nWorkers)
  }

  /** Do a single test. */
  def doTest = {
    val (f, p, a, b, nWorkers) = pickParams
    val seqResult = estimate(f, a, b)
    val concResult = new Adaptive(f, a, b, Epsilon, nWorkers)()
    assert(
      seqResult != 0.0 && Math.abs((seqResult-concResult)/seqResult) < 1E-7 ||
        Math.abs(seqResult-concResult) < 1E-10,
      "failed\nf = "+TrapeziumTest.toString(p)+"\n"+
        "a = "+a+"; b = "+b+"; nWorkers = "+nWorkers+"\n"+
        "seqResult = "+seqResult+"; concResult = "+concResult)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){
      doTest; if(i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}


