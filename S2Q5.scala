import io.threadcso._
import scala.collection.mutable.Queue

class Server {

    // type for returning message to client (channel for communication)
    private type replyChan = Chan[String]
    // a message consists of whether user is a man or woman (woman is true, man is false),
    // their name and the reply channel
    private type message = (Boolean, String, replyChan)
    // a person is stored by name and their reply channel
    private type person = (String, replyChan)
    // channel for requesting a pair
    private val requestPairChan = ManyOne[message]
    // channel for shutting down the server
    private val shutdownChan = ManyOne[Unit]

    private def server = proc {
        val men = Queue[person]()
        val women = Queue[person]()
        serve (
            requestPairChan =?=> { m =>
                val (sex, name1, replyChan1) = m
                // if request is made by a woman
                if (sex) {
                    // if available man, create pairing
                    if (!men.isEmpty) {
                        val (name2, replyChan2) = men.dequeue
                        replyChan1!name2
                        replyChan2!name1
                    }
                    // otherwise add woman to the queue
                    else women.enqueue((name1, replyChan1))
                }
                // do the same in reverse for mean
                else {
                    // if available woman, create pairing
                    if (!women.isEmpty) {
                        val (name2, replyChan2) = women.dequeue
                        replyChan1!name2
                        replyChan2!name1
                    }
                    // otherwise add man to the queue
                    else men.enqueue((name1, replyChan1))
                }
            }

            // if receive shutdown request close the server
            | shutdownChan =?=> { _ => {
                    requestPairChan.close
                    shutdownChan.close
                    for ((_, chan) <- (men ++ women)) chan.close
                }
            }
        )
    }

    // Fork off the server
    server.fork

    // function for male client to request a pairing - in the case the server shuts down before pairing made, return N/A
    def manSync(me: String): String = {
        attempt {
            val replyChan = OneOne[String]
            val m: message = (false, me, replyChan)
            requestPairChan!m
            replyChan?()
        }
        { "N/A" }
    }

    // function for female client to request a pairing - in the case the server shuts down before pairing made, return N/A
    def womanSync(me: String): String = {
        attempt {
            val replyChan = OneOne[String]
            val m: message = (true, me, replyChan)
            requestPairChan!m
            replyChan?()
        }
        { "N/A" }
    }

    // shutdown the server
    def shutdown = shutdownChan!()

}

import scala.util.Random

object serverTest {
    // N is number of clients, time is time server is run for
    private val N = 50; private val time = 1

    private var pairs = List[pair]()

    private var myServer = new Server

    // to communicate between clients and test - for simplicity women are first string
    private type pair = (String, String)
    val pairChan = ManyOne[pair]

    def receivePairs = proc {
        repeat {
            pairs = pairChan?()::pairs
        }
    }

    def terminateServer = proc {
        Thread.sleep(time)
        myServer.shutdown
        pairChan.close
    }

    // process to simulate a client - will request a pair then send the pairing to the test method
    // true for women, false for men
    def client(me: String, sex: Boolean) = proc {
        attempt {
            if (sex) {
                val result = myServer.manSync(me)
                pairChan!(result, me)
            }
            else {
                val result = myServer.womanSync(me)
                pairChan!(me, result)
            }
        }
        {}
    }

    // checks if every possible pair has been formed (either only men or only women without a pair)
    // and checks each pair has exactly one man and one woman
    def checkPairs(men: Array[String], women: Array[String]): Boolean = {
        var womenUnpaired = false
        var menUnpaired = false
        for ((man, woman) <- pairs) {
            if (man == "N/A") womenUnpaired = true
            if (woman == "N/A") menUnpaired = true
            if ((!men.contains(man)) && (man != "N/A")) false
            if ((!women.contains(woman)) && (woman != "N/A")) false
        }
        if (menUnpaired && womenUnpaired) false
        true
    }

    def doTest = {
        myServer = new Server
        val numberOfMen = Random.nextInt(N)
        val men = Array.fill(numberOfMen)(Random.alphanumeric.take(10).mkString)
        val women = Array.fill(N - numberOfMen)(Random.alphanumeric.take(10).mkString)
        val clients = (|| (for (person <- men) yield client(person, false))) || (|| (for (person <- women) yield client(person, true)))
        run (clients || receivePairs || terminateServer)
        assert(checkPairs(men, women))
    }

    def main(args: Array[String]) = {
        for(i <- 0 until 1000) {
            doTest; if(i%10 == 0) print(".")
        }
        println; io.threadcso.exit
    }
}