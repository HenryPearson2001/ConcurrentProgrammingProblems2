# ConcurrentProgrammingProblems2
University concurrent programming problems

Problems:


Question 4
Recall the trapezium rule, used for estimating integrals, discussed in the
third chapter. Adaptive quadrature is an alternative approach that proceeds as follows. In
order to calculate 􏰆 b f (x) dx, first compute the midpoint mid = (a + b)/2. Then estimate a
three integrals, 􏰆 mid f (x) dx, 􏰆 b f (x) dx and 􏰆 b f (x) dx, each using the trapezium rule a mid a
with a single interval. If the sum of the former two estimates is within some value ε of the third, then we take that third estimate as being the result. Otherwise, recursively estimate the integrals over the two ranges a to mid and mid to b, and sum the results. The following sequential algorithm captures this idea:
/∗∗ Estimate the integral of f from a to b using adaptive quadrature. ∗/ def estimate(f: Double => Double, a: Double, b: Double) : Double = {
val mid = (a+b)/2.0
val fa = f(a); val fb = f(b); val fmid = f(mid)
val lArea = (fa+fmid)∗(mid−a)/2; val rArea = (fmid+fb)∗(b−mid)/2 val area = (fa+fb)∗(b−a)/2
if (Math.abs(lArea+rArea−area) < Epsilon) area else estimate(f,a,mid) + estimate(f,mid,b)
}
Write a concurrent program to implement adaptive quadrature. Give your program the following signature:
class Adaptive(f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){ require(a <= b)
def apply(): Double = ... }
The program should use a bag of tasks with replacement: the two recursive calls in the sequential version can be implemented by returning tasks to the bag. The bag of tasks needs to store the tasks still to be performed: use either a Queue or a Stack for this. You will have to think carefully about when the system should terminate.
A test harness for this program is on the course website (making use of the class TrapeziumTest from lectures). Use this to test your code.
Suggest ways in which your program can be made more efficient; optional: implement them.
2
Question 5
Consider the following synchronisation problem. There are two types of client process, which we shall call men and women. These processes need to pair off for some purpose, with each pair containing one process of each type. Design a server process to support this. Each client should send its name to the server, and receive back the name of its partner. Encapsulate the server within a class, with public methods
def manSync(me: String): String = ... def womanSync(me: String): String = ...
Implement a test rig for your implementation: think carefully about the correctness condition.
Question 6
Suppose n processes are connected in a ring. Each process i (for i = 0, . . . , n − 1) initially holds a value xi : T . Write a concurrent program so that each process ends up in possession of the value
f(f(f(. . . f(x0, x1), x2) . . .), xn−1),
for some given function f; or using functional programming notation:
foldl1 f List(x0, x1, . . . , xn−1)
You should state properties that hold at key points of your program, for example concerning the values passed on channels.
Test your code.
What property of f will allow a different program that achieves the same goal? Ex- plain your answer, and outline how this can be achieved. (A full implementation is not necessary.)
