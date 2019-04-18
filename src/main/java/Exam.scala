import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Flip, Select, Universe}
import com.cra.figaro.library.compound.CPD

object Exam {
  Universe.createNew()

  private val difficulty = Flip(0.6)

  private val intelligence = Flip(0.7)

  private val grade = CPD(intelligence, difficulty,
    (false, false) -> Select(0.3 -> 1, 0.4 -> 2, 0.3 -> 3),
    (false, true) -> Select(0.05 -> 1, 0.25 -> 2, 0.7 -> 3),
    (true, false) -> Select(0.9 -> 1, 0.08 -> 2, 0.02 -> 3),
    (true, true) -> Select(0.5 -> 1, 0.3 -> 2, 0.2 -> 3)
  )

  private val SAT = CPD(intelligence,
    false -> Flip(0.95),
    true -> Flip(0.2)
  )

  private val letter = CPD(grade,
    1 -> Flip(0.1),
    2 -> Flip(0.4),
    3 -> Flip(0.99)
  )

  def main(args: Array[String]) {
    letter.observe(true)
    val alg = VariableElimination(letter, SAT, grade, intelligence, difficulty)
    alg.start()
    val v1 = alg.probability(intelligence, true)
    println("Probability of intelligence: " + v1)
    val v2 = alg.probability(SAT, true)
    println("Probability of SAT: " + v2)
    val v3 = alg.probability(grade, 1)
    println("Probability of grade: " + v3)
    val v4 = alg.probability(difficulty, false)
    println("Probability of difficulty: " + v4)
    val v5 = alg.probability(letter, true)
    println("Probability of letter: " + v5)

    val prob = v1 * v2 * v3 * v4 * v5
    println("All:" + prob)

    alg.kill
  }
}
