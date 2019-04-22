import Burglary.{burglary, earthquake, johnCalls}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Flip, Select, Universe}
import com.cra.figaro.library.compound.CPD

object Income extends Enumeration {
  val High, Medium, Low = Value
}

object Age extends Enumeration {
  val Young, MiddleAged, Old = Value
}

object RodToIncome extends Enumeration {
  val High, Low = Value
}

object Assets extends Enumeration {
  val High, Medium, Low = Value
}

object PaymentHistory extends Enumeration {
  val Excellent, Acceptable, Unacceptable = Value
}

object FutureIncome extends Enumeration {
  val Promising, NotPromising = Value
}

object Reliability extends Enumeration {
  val Reliable, Unreliable = Value
}

object CreditWorthiness extends Enumeration {
  val Positive, Negative = Value
}

object Education extends Enumeration {
  val NoEducation, HighSchool, University = Value
}

object MaritalStatus extends Enumeration {
  val Married, Alone = Value
}

object CriminalHistory extends Enumeration {
  val Criminal, NonCriminal = Value
}


object Loan {
  Universe.createNew()

  private val income = Select(0.2 -> Income.High, 0.5 -> Income.Medium, 0.3 -> Income.Low)

  private val age = Select(0.2 -> Age.Young, 0.7 -> Age.MiddleAged, 0.1 -> Age.Old)

  private val rodToIncome = Select(0.3 -> RodToIncome.Low, 0.7 -> RodToIncome.High)

  private val education = CPD(age,
    Age.Young-> Select(0.1 -> Education.NoEducation, 0.89 -> Education.HighSchool, 0.01 -> Education.University),
    Age.MiddleAged-> Select(0.1 -> Education.NoEducation, 0.6 -> Education.HighSchool, 0.3 -> Education.University),
    Age.Old-> Select(0.1 -> Education.NoEducation, 0.5 -> Education.HighSchool, 0.4 -> Education.University)
  )

  private val maritalStatus = CPD(age,
    Age.Young-> Select(0.2 -> MaritalStatus.Married, 0.8 -> MaritalStatus.Alone),
    Age.MiddleAged-> Select(0.5 -> MaritalStatus.Married, 0.5 -> MaritalStatus.Alone),
    Age.Old-> Select(0.8 -> MaritalStatus.Married, 0.2 -> MaritalStatus.Alone)
  )

  private val criminalHistory = CPD(age, income,
    (Age.Young, Income.High) -> Select(0.95 -> CriminalHistory.NonCriminal, 0.05 -> CriminalHistory.Criminal),
    (Age.Young, Income.Medium) -> Select(0.97 -> CriminalHistory.NonCriminal, 0.03 -> CriminalHistory.Criminal),
    (Age.Young, Income.Low) -> Select(0.85 -> CriminalHistory.NonCriminal, 0.15 -> CriminalHistory.Criminal),
    (Age.MiddleAged, Income.High) -> Select(0.93 -> CriminalHistory.NonCriminal, 0.07 -> CriminalHistory.Criminal),
    (Age.MiddleAged, Income.Medium) -> Select(0.9 -> CriminalHistory.NonCriminal, 0.1 -> CriminalHistory.Criminal),
    (Age.MiddleAged, Income.Low) -> Select(0.8 -> CriminalHistory.NonCriminal, 0.2 -> CriminalHistory.Criminal),
    (Age.Old, Income.High) -> Select(0.9 -> CriminalHistory.NonCriminal, 0.1 -> CriminalHistory.Criminal),
    (Age.Old, Income.Medium) -> Select(0.85 -> CriminalHistory.NonCriminal, 0.15 -> CriminalHistory.Criminal),
    (Age.Old, Income.Low) -> Select(0.7 -> CriminalHistory.NonCriminal, 0.3 -> CriminalHistory.Criminal)
  )

  private val assets = CPD(income, maritalStatus,
    (Income.High, MaritalStatus.Married) -> Select(0.7 -> Assets.High, 0.2 -> Assets.Medium, 0.1 -> Assets.Low),
    (Income.Medium, MaritalStatus.Married) -> Select(0.4 -> Assets.High, 0.4 -> Assets.Medium, 0.2 -> Assets.Low),
    (Income.Low, MaritalStatus.Married) -> Select(0.2 -> Assets.High, 0.4 -> Assets.Medium, 0.4 -> Assets.Low),
    (Income.High, MaritalStatus.Alone) -> Select(0.6 -> Assets.High, 0.3 -> Assets.Medium, 0.1 -> Assets.Low),
    (Income.Medium, MaritalStatus.Alone) -> Select(0.3 -> Assets.High, 0.4 -> Assets.Medium, 0.3 -> Assets.Low),
    (Income.Low, MaritalStatus.Alone) -> Select(0.1 -> Assets.High, 0.4 -> Assets.Medium, 0.5 -> Assets.Low)
  )

  private val paymentHistory = CPD(age, rodToIncome,
    (Age.Young, RodToIncome.Low) -> Select(0.3 -> PaymentHistory.Excellent, 0.4 -> PaymentHistory.Acceptable, 0.3 -> PaymentHistory.Unacceptable),
    (Age.Young, RodToIncome.High) -> Select(0.2 -> PaymentHistory.Excellent, 0.5 -> PaymentHistory.Acceptable, 0.3 -> PaymentHistory.Unacceptable),
    (Age.MiddleAged, RodToIncome.Low) -> Select(0.6 -> PaymentHistory.Excellent, 0.2 -> PaymentHistory.Acceptable, 0.2 -> PaymentHistory.Unacceptable),
    (Age.MiddleAged, RodToIncome.High) -> Select(0.4 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.3 -> PaymentHistory.Unacceptable),
    (Age.Old, RodToIncome.Low) -> Select(0.8 -> PaymentHistory.Excellent, 0.15 -> PaymentHistory.Acceptable, 0.05 -> PaymentHistory.Unacceptable),
    (Age.Old, RodToIncome.High) -> Select(0.7 -> PaymentHistory.Excellent, 0.2 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable)
  )


  private val futureIncome = CPD(assets, income,
    (Assets.High, Income.High) -> Select(0.9 -> FutureIncome.Promising, 0.1 -> FutureIncome.NotPromising),
    (Assets.High, Income.Medium) -> Select(0.8 -> FutureIncome.Promising, 0.2 -> FutureIncome.NotPromising),
    (Assets.High, Income.Low) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.High) -> Select(0.7 -> FutureIncome.Promising, 0.3 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.Medium) -> Select(0.8 -> FutureIncome.Promising, 0.2 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.Low) -> Select(0.5 -> FutureIncome.Promising, 0.5 -> FutureIncome.NotPromising),
    (Assets.Low, Income.High) -> Select(0.3 -> FutureIncome.Promising, 0.7 -> FutureIncome.NotPromising),
    (Assets.Low, Income.Medium) -> Select(0.2 -> FutureIncome.Promising, 0.8 -> FutureIncome.NotPromising),
    (Assets.Low, Income.Low) -> Select(0.1 -> FutureIncome.Promising, 0.9 -> FutureIncome.NotPromising)
  )

  private val reliability = CPD(paymentHistory, age, education,
    (PaymentHistory.Excellent, Age.Young, Education.University) -> Select(0.75 -> Reliability.Reliable, 0.25 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.MiddleAged, Education.University) -> Select(0.85 -> Reliability.Reliable, 0.15 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.Old, Education.University) -> Select(0.9 -> Reliability.Reliable, 0.1 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Young, Education.University) -> Select(0.4 -> Reliability.Reliable, 0.6 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.MiddleAged, Education.University) -> Select(0.5 -> Reliability.Reliable, 0.5 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Old, Education.University) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Young, Education.University) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.MiddleAged, Education.University) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Old, Education.University) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable),

    (PaymentHistory.Excellent, Age.Young, Education.HighSchool) -> Select(0.7 -> Reliability.Reliable, 0.3 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.MiddleAged, Education.HighSchool) -> Select(0.8 -> Reliability.Reliable, 0.2 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.Old, Education.HighSchool) -> Select(0.85 -> Reliability.Reliable, 0.15 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Young, Education.HighSchool) -> Select(0.35 -> Reliability.Reliable, 0.65 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.MiddleAged, Education.HighSchool) -> Select(0.35 -> Reliability.Reliable, 0.55 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Old, Education.HighSchool) -> Select(0.55 -> Reliability.Reliable, 0.45 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Young, Education.HighSchool) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.MiddleAged, Education.HighSchool) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Old, Education.HighSchool) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable),

    (PaymentHistory.Excellent, Age.Young, Education.NoEducation) -> Select(0.65 -> Reliability.Reliable, 0.35 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.MiddleAged, Education.NoEducation) -> Select(0.75 -> Reliability.Reliable, 0.25 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.Old, Education.NoEducation) -> Select(0.8 -> Reliability.Reliable, 0.2 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Young, Education.NoEducation) -> Select(0.3 -> Reliability.Reliable, 0.7 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.MiddleAged, Education.NoEducation) -> Select(0.4 -> Reliability.Reliable, 0.6 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Old, Education.NoEducation) -> Select(0.5 -> Reliability.Reliable, 0.5 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Young, Education.NoEducation) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.MiddleAged, Education.NoEducation) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Old, Education.NoEducation) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable)
  )

  private val creditWorthiness = CPD(reliability, futureIncome, rodToIncome, criminalHistory,
    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.Low, CriminalHistory.NonCriminal) -> Select(0.95 -> CreditWorthiness.Positive, 0.05 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.High, CriminalHistory.NonCriminal) -> Select(0.9 -> CreditWorthiness.Positive, 0.1 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.Low, CriminalHistory.NonCriminal) -> Select(0.55 -> CreditWorthiness.Positive, 0.45 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.High, CriminalHistory.NonCriminal) -> Select(0.3 -> CreditWorthiness.Positive, 0.7 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.Low, CriminalHistory.NonCriminal) -> Select(0.5 -> CreditWorthiness.Positive, 0.5 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.High, CriminalHistory.NonCriminal) -> Select(0.2 -> CreditWorthiness.Positive, 0.8 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.Low, CriminalHistory.NonCriminal) -> Select(0.1 -> CreditWorthiness.Positive, 0.9 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.High, CriminalHistory.NonCriminal) -> Select(0.05 -> CreditWorthiness.Positive, 0.95 -> CreditWorthiness.Negative),

    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.Low, CriminalHistory.Criminal) -> Select(0.4 -> CreditWorthiness.Positive, 0.6 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.High, CriminalHistory.Criminal) -> Select(0.3 -> CreditWorthiness.Positive, 0.7 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.Low, CriminalHistory.Criminal) -> Select(0.15 -> CreditWorthiness.Positive, 0.85 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.High, CriminalHistory.Criminal) -> Select(0.1 -> CreditWorthiness.Positive, 0.9 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.Low, CriminalHistory.Criminal) -> Select(0.005 -> CreditWorthiness.Positive, 0.095 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.High, CriminalHistory.Criminal) -> Select(0.002 -> CreditWorthiness.Positive, 0.098 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.Low, CriminalHistory.Criminal) -> Select(0.001 -> CreditWorthiness.Positive, 0.999 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.High, CriminalHistory.Criminal) -> Select(0.001 -> CreditWorthiness.Positive, 0.999 -> CreditWorthiness.Negative)

  )

  def main(args: Array[String]) {

    reliability.observe(Reliability.Reliable)
    paymentHistory.observe(PaymentHistory.Excellent)
    val alg = VariableElimination(creditWorthiness)
    alg.start()
    println("Reliable client with Excellent History: " + alg.probability(creditWorthiness, CreditWorthiness.Positive))
    alg.kill
    unobserve()

    reliability.observe(Reliability.Reliable)
    paymentHistory.observe(PaymentHistory.Acceptable)
    val alg2 = VariableElimination(creditWorthiness)
    alg2.start()
    println("Reliable client with Acceptable History: " + alg2.probability(creditWorthiness, CreditWorthiness.Positive))
    alg2.kill
    unobserve()

    reliability.observe(Reliability.Reliable)
    paymentHistory.observe(PaymentHistory.Unacceptable)
    val alg3 = VariableElimination(creditWorthiness)
    alg3.start()
    println("Reliable client with Unacceptable History: " + alg3.probability(creditWorthiness, CreditWorthiness.Positive))
    alg3.kill
    unobserve()

    futureIncome.observe(FutureIncome.NotPromising)
    paymentHistory.observe(PaymentHistory.Unacceptable)
    rodToIncome.observe(RodToIncome.High)
    val alg4 = VariableElimination(creditWorthiness)
    alg4.start()
    println("High Ratio of Debts, Not Promising Future income and Unacceptable Payment History: " + alg4.probability(creditWorthiness, CreditWorthiness.Positive))
    alg4.kill
    unobserve()

    criminalHistory.observe(CriminalHistory.Criminal)
    paymentHistory.observe(PaymentHistory.Unacceptable)
    val alg5 = VariableElimination(creditWorthiness)
    alg5.start()
    println("Criminal with Unacceptable payment history: " + alg5.probability(creditWorthiness, CreditWorthiness.Positive))
    alg5.kill
  }

  def unobserve(): Unit = {
    income.unobserve()
    age.unobserve()
    rodToIncome.unobserve()
    education.unobserve()
    maritalStatus.unobserve()
    criminalHistory.unobserve()
    assets.unobserve()
    reliability.unobserve()
    futureIncome.unobserve()
    paymentHistory.unobserve()
    creditWorthiness.unobserve()
  }


}
