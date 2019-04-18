import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.^^

/**
  * A Markov logic example.
  */
object Smokers {
  private class Person {
    val smokes = Flip(0.6)
  }

  private val alice, bob, clara = new Person
  private val friends = List((alice, bob), (bob, clara))
  clara.smokes.observe(true)

  private def smokingInfluence(pair: (Boolean, Boolean)) =
    if (pair._1 == pair._2) 3.0; else 1.0

  for { (p1, p2) <- friends } {
    ^^(p1.smokes, p2.smokes).setConstraint(smokingInfluence)
  }

  def main(args: Array[String]) {
    val alg = MetropolisHastings(20000, ProposalScheme.default, alice.smokes)
    alg.start()
    println("Probability of Alice smoking: " + alg.probability(alice.smokes, true))
    alg.kill
  }
}