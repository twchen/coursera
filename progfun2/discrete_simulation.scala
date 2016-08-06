object Simulator {
  trait Simulation {
    // doesn't take any parameters and returns Unit
    type Action = () => Unit
    // an event consists of an action and the time when it must be produced.
    case class Event(time: Int, action: Action)

    private type Agenda = List[Event]
    // agenda is a list of events sorted in ascending order of time
    private var agenda: Agenda = List()

    private var curtime = 0
    def currentTime: Int = curtime

    // register an action to perform after a certain delay
    // i.e. inserts the task Event(curtime + dalay, () => block) into the agenda list
    def afterDelay(delay: Int)(block: => Unit): Unit = {
      val item = Event(curtime + delay, () => block)
      agenda = item :: agenda sortWith (_.time < _.time)
    }

    private def loop(): Unit = {
      while (!agenda.isEmpty) {
        val first = agenda.head
        curtime = first.time
        first.action()
        agenda = agenda.tail
      }
    }
    // performs the simulation until there are no more action waiting
    def run(): Unit = {
      afterDelay(0) {
        println(s"*** simulation started, time = $curtime ***")
      }
      loop()
    }

  }

  abstract class Gates extends Simulation {
    def InverterDelay: Int
    def AndGateDelay: Int
    def OrGateDelay: Int

    class Wire {
      private var sigVal = false
      private var actions: List[Action] = List()
      def getSignal: Boolean = sigVal
      def setSignal(s: Boolean): Unit = {
        if (s != sigVal) {
          sigVal = s
          actions foreach (_())
        }
      }

      // attaches a new action to the wire. All attached actions are executed at each change of sigVal
      def addAction(a: Action): Unit = {
        actions = a :: actions
        a()
      }
    }

    def inverter(input: Wire, output: Wire): Unit = {
      def invertAction(): Unit = {
        val inputSig = input.getSignal
        afterDelay(InverterDelay) { output setSignal !inputSig }
      }
      input addAction invertAction
    }

    def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
      def andAction(): Unit = {
        val in1Sig = in1.getSignal
        val in2Sig = in2.getSignal
        afterDelay(AndGateDelay) { output setSignal (in1Sig & in2Sig) }
      }
      in1 addAction andAction
      in2 addAction andAction
    }

    def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
      def orAction(): Unit = {
        val in1Sig = in1.getSignal
        val in2Sig = in2.getSignal
        afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig) }
      }
      in1 addAction orAction
      in2 addAction orAction
    }

    def probe(name: String, wire: Wire): Unit = {
      def probeAction(): Unit = {
        println(s"$name $currentTime value = ${wire.getSignal}")
      }
      wire addAction probeAction
    }

  }

  abstract class Circuits extends Gates {
    def halfAdder(a: Wire, b: Wire, sum: Wire, carry: Wire): Unit = {
      val c, d = new Wire
      andGate(a, b, carry) // carry = a AND b
      orGate(a, b, c)
      inverter(carry, d)
      andGate(c, d, sum) // sum = (a OR b) AND (NOT (a and b))
    }

    def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
      val s, c1, c2 = new Wire
      halfAdder(a, cin, s, c1)
      halfAdder(b, s, sum, c2)
      orGate(c1, c2, cout)
    }
  }

  trait Parameters {
    def InverterDelay = 2
    def AndGateDelay = 3
    def OrGateDelay = 5
  }

  object sim extends Circuits with Parameters
}