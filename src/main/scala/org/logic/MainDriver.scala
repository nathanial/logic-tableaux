package logic
import logic.tests.TestSentential
import logic.tests.TestPredicate

object MainDriver extends Application {
   benchmark(1) {
     TestSentential.run
     TestPredicate.run
   }

  def benchmark(numberOfTimes: int)(func: => Unit){
    val before = System.currentTimeMillis

    for(i <- 1 to numberOfTimes)
      func //execute function

    val after = System.currentTimeMillis
    println("Time elapsed = " + (after - before) / 1000.0)
  }
}
