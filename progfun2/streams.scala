trait Stream[+T] extends Seq[T] {
    def isEmpty: Boolean
    def head: T
    def tail: Stream[T]
}

object Stream {
    def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
        def isEmpty = false
        def head = hd
        lazy val tail = tl
    }
    def empty = new Stream[Nothing] {
        def isEmpty = true
        def head = throw new NoSuchElementException("Stream.empty.head")
        def tail = throw new NoSuchElementException("Stream.empty.tail")
    }
}

def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if(lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
}

// stream of integers starting from n
def from(n: Int): Stream[Int] = n #:: from(n + 1)

def sieve(s: Stream[Int]): Stream[Int] = 
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

// all prime numbers
val primes = sieve(from(2))

def sqrtStream(x: Double): Double = {
    def imporve(guess: Double) = (guess + x / guess) / 2
    def isGoodEnough(guess: Double) = math.abs(guess * guess - x) < 1e-6 * x
    lazy val guesses: Stream[Double] = 1 #:: (guesses map imporve)
    guesses.filter(isGoodEnough).head
}
