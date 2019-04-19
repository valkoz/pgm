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


object Loan {
  Universe.createNew()

  private val income = Select(0.2 -> Income.High, 0.5 -> Income.Medium, 0.3 -> Income.Low)

  private val age = Select(0.2 -> Age.Young, 0.7 -> Age.MiddleAged, 0.1 -> Age.Old)

  private val rodToIncome = Select(0.3 -> RodToIncome.Low, 0.7 -> RodToIncome.High)

  private val assets = CPD(income,
    Income.High -> Select(0.6 -> Assets.High, 0.3 -> Assets.Medium, 0.1 -> Assets.Low),
    Income.Medium  -> Select(0.3 -> Assets.High, 0.4 -> Assets.Medium, 0.3 -> Assets.Low),
    Income.Low  -> Select(0.1 -> Assets.High, 0.4 -> Assets.Medium, 0.5 -> Assets.Low)
  )

  private val paymentHistory = CPD(age, rodToIncome,
    (Age.Young, RodToIncome.Low) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable),
    (Age.Young, RodToIncome.High) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable),
    (Age.MiddleAged, RodToIncome.Low) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable),
    (Age.MiddleAged, RodToIncome.High) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable),
    (Age.Old, RodToIncome.Low) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable),
    (Age.Old, RodToIncome.High) -> Select(0.6 -> PaymentHistory.Excellent, 0.3 -> PaymentHistory.Acceptable, 0.1 -> PaymentHistory.Unacceptable)
  )


  private val futureIncome = CPD(assets, income,
    (Assets.High, Income.High) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.High, Income.Medium) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.High, Income.Low) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.High) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.Medium) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Medium, Income.Low) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Low, Income.High) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Low, Income.Medium) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising),
    (Assets.Low, Income.Low) -> Select(0.6 -> FutureIncome.Promising, 0.4 -> FutureIncome.NotPromising)
  )

  private val reliability = CPD(paymentHistory, age,
    (PaymentHistory.Excellent, Age.Young) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.MiddleAged) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.Old) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Young) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.MiddleAged) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Old) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Young) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.MiddleAged) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Old) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable)
  )

  private val creditWorthiness = CPD(reliability, futureIncome, rodToIncome,
    (Reliability.Reliable, FutureIncome.Promising,RodToIncome.Low) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.High) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising,RodToIncome.Low) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.High) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising,RodToIncome.Low) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.High) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising,RodToIncome.Low) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.High) -> Select(0.6 -> CreditWorthiness.Positive, 0.4 -> CreditWorthiness.Negative),

  )


}
