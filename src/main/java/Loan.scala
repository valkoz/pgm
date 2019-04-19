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

  private val reliability = CPD(paymentHistory, age,
    (PaymentHistory.Excellent, Age.Young) -> Select(0.75 -> Reliability.Reliable, 0.25 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.MiddleAged) -> Select(0.85 -> Reliability.Reliable, 0.15 -> Reliability.Unreliable),
    (PaymentHistory.Excellent, Age.Old) -> Select(0.9 -> Reliability.Reliable, 0.1 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Young) -> Select(0.4 -> Reliability.Reliable, 0.6 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.MiddleAged) -> Select(0.5 -> Reliability.Reliable, 0.5 -> Reliability.Unreliable),
    (PaymentHistory.Acceptable, Age.Old) -> Select(0.6 -> Reliability.Reliable, 0.4 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Young) -> Select(0.01 -> Reliability.Reliable, 0.99 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.MiddleAged) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable),
    (PaymentHistory.Unacceptable, Age.Old) -> Select(0.1 -> Reliability.Reliable, 0.9 -> Reliability.Unreliable)
  )

  private val creditWorthiness = CPD(reliability, futureIncome, rodToIncome,
    (Reliability.Reliable, FutureIncome.Promising,RodToIncome.Low) -> Select(0.95 -> CreditWorthiness.Positive, 0.05 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.Promising, RodToIncome.High) -> Select(0.9 -> CreditWorthiness.Positive, 0.1 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising,RodToIncome.Low) -> Select(0.55 -> CreditWorthiness.Positive, 0.45 -> CreditWorthiness.Negative),
    (Reliability.Reliable, FutureIncome.NotPromising, RodToIncome.High) -> Select(0.3 -> CreditWorthiness.Positive, 0.7 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising,RodToIncome.Low) -> Select(0.5 -> CreditWorthiness.Positive, 0.5 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.Promising, RodToIncome.High) -> Select(0.2 -> CreditWorthiness.Positive, 0.8 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising,RodToIncome.Low) -> Select(0.1 -> CreditWorthiness.Positive, 0.9 -> CreditWorthiness.Negative),
    (Reliability.Unreliable, FutureIncome.NotPromising, RodToIncome.High) -> Select(0.05 -> CreditWorthiness.Positive, 0.95 -> CreditWorthiness.Negative),

  )


}
