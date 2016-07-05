// while loop
def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  } else {}
}

// REPEAT 1st version
def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) {}
  else REPEAT(command)(condition)
}

// repeat 2nd version
def repeat(command: => Unit) = new {
  def until(condition: => Boolean) = {
    do {
      command
    } while (!condition)
  }
}

// example use
def factorial(n: Int): Int = {
  var product = 1
  var i = 1
  repeat {
    product *= i
    i += 1
  } until { i > n }
  product
}
