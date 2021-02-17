import io.threadcso._
import scala.util.Random

class ring(f:(Int, Int) => Int, N: Int, Max: Int) {

    // standard process in the ring - initially ith process gets passed foldl f List(x0,...,x(i-1)) and then gets
    // passed foldl f List(x0,...,x(N-1))
    private def process(initialValue: Int, in: ?[Int], out: ![Int]) = proc {
        var value = initialValue
        // get foldl f List(x0,...,x(i-1))
        value = in?()
        value = f(value, initialValue)
        // send foldl f List(x0,...,xi)
        out!(value)
        // get foldl f List(x0,...,x(N-1))
        value = in?()
        // send the value to the next process - may be sent to the initial process which will be closed so use attempt
        attempt {
            out!(value)
        }
        {}
        in.closeIn
        out.closeOut
        println(value)
    }

    private def initialProcess(initialValue: Int, in: ?[Int], out: ![Int]) = proc {
        var value = initialValue
        // send x0
        out!(value)
        // receive foldl f List(x0,...,x(N-1))
        value = in?()
        // send out to circulate
        out!(value)
        in.closeIn
        out.closeOut
        println(value)
    }

    def setUpRing: PROC = {
        val channels = Array.fill(N)(OneOne[Int])
        val processes = (|| (for (i <- 0 until N - 1) yield process(Random.nextInt(Max), channels(i), channels(i + 1)))) || initialProcess(Random.nextInt(Max), channels(N-1), channels(0))
        processes
    }
}

object ringTest {

    private def f(input: (Int, Int)): Int = {
        var (x, y) = input
        x * y + x + y
    }

    private def doTest = {
        val myRing = new ring(f(_,_), 10, 5)
        val processes = myRing.setUpRing
        run(processes)
    }

    def main(args: Array[String]) = {
        doTest
    }
}