import com.cra.figaro.language.{Flip, Select, Universe}
import com.cra.figaro.library.compound.CPD

object Loan {
  Universe.createNew()

  private val income = Select(0.2 -> "High", 0.5 -> "Medium", 0.3 -> "Low")

  private val age = Select(0.2 -> "18..25", 0.7 -> "25..60", 0.1 -> "60+")

  private val rodToIncome = Select(0.3 -> "LowR", 0.7 -> "HighR")

  private val assets = CPD(income,
    "High" -> Select(0.6 -> "HighA", 0.3 -> "MediumA", 0.1 -> "LowA"),
    "Medium" -> Select(0.3 -> "HighA", 0.4 -> "MediumA", 0.3 -> "LowA"),
    "Low" -> Select(0.1 -> "HighA", 0.4 -> "MediumA", 0.5 -> "LowA")
  )

  private val paymentHistory = CPD(age, rodToIncome,
    ("18..25", "LowR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable"),
    ("18..25", "HighR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable"),
    ("25..60", "LowR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable"),
    ("25..60", "HighR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable"),
    ("60+", "LowR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable"),
    ("60+", "HighR") -> Select(0.6 -> "Excellent", 0.3 -> "Acceptable", 0.1 -> "Unacceptable")
  )


  private val futureIncome = CPD(assets, income,
    ("HighA", "High") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("HighA", "Medium") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("HighA", "Low") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("MediumA", "High") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("MediumA", "Medium") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("MediumA", "Low") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("LowA", "High") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("LowA", "Medium") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising"),
    ("LowA", "Low") -> Select(0.6 -> "Promising", 0.4 -> "Not_promising")
  )

  private val reliability = CPD(paymentHistory, age,
    ("Excellent", "18..25") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Excellent", "25..60") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Excellent", "60+") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Acceptable", "18..25") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Acceptable", "25..60") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Acceptable", "60+") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Unacceptable", "18..25") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Unacceptable", "25..60") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable"),
    ("Unacceptable", "60+") -> Select(0.6 -> "Reliable", 0.4 -> "Unreliable")
  )

  private val creditWorthiness = CPD(reliability, futureIncome, rodToIncome,
    ("Reliable", "Promising","LowR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Reliable", "Promising","HighR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Reliable", "Not_promising","LowR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Reliable", "Not_promising","HighR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Unreliable", "Promising","LowR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Unreliable", "Promising","HighR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Unreliable", "Not_promising","LowR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),
    ("Unreliable", "Not_promising","HighR") -> Select(0.6 -> "Positive", 0.4 -> "Negative"),

  )


}
