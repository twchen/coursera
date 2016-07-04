private var uidCount = 0L
def getUniqueId(): Long = {
    uidCount = uidCount + 1
    uidCount
}
def startThread() = {
    val t = new Thread {
        override def run() {
            val uids = for (i <- 0 until 10) yield getUniqueId()
            println(uids)
        }
    }
    t.start
    t
}
startThread();startThread()
// result
// Vector(1, 2, 3, 5, 7, 9, 11, 13, 15, 17)
// Vector(1, 4, 6, 8, 10, 12, 14, 16, 18, 19)


private val x = new AnyRef{}
private var uidCount = 0L
def getUniqueId(): Long = x synchronized {
    uidCount = uidCount + 1
    uidCount
}
def startThread() = {
    val t = new Thread {
        override def run() {
            val uids = for (i <- 0 until 10) yield getUniqueId()
            println(uids)
        }
    }
    t.start
    t
}
startThread();startThread()

// result
// Vector(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
// Vector(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
