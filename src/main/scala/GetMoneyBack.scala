package se.ramn.getmoneyback

import java.util.Scanner
import BigDecimal.RoundingMode
import io.StdIn.readLine
import GetMoneyBack.Person
import GetMoneyBack.Amount


object GetMoneyBack {
  type Person = String
  type Amount = BigDecimal

  def main(args: Array[String]) {
    unsafePrintBanner()
    val expenses = parseInputs(unsafeReadInputs())
    if (!inputIsValid(expenses)) {
      throw new IllegalArgumentException("Names must be unique")
    }
    calcDebts(expenses) foreach println
  }

  def unsafePrintBanner() = {
    System.err.println("Enter a number of rows with names and expenses")
    System.err.println("Input format: <name> <expense>")
    System.err.println("End input by inserting a blank line.")
    System.err.println()
    System.err.println()
  }

  def unsafeReadInputs(): List[String] = {
    Iterator.continually(readLine).takeWhile(_ != "").toList
  }

  def parseInputs(inputs: List[String]): List[Expense] = {
    inputs.map { expenseInput: String =>
      try {
        val scanner = new Scanner(expenseInput)
        val person = scanner.next
        val amount = BigDecimal(scanner.next)
        require(amount >= 0, "Amount must be positive")
        Expense(person, amount)
      } catch {
        case e: Exception =>
          System.err.println("Error parsing input row: ", e)
          throw e
      }
    }
  }

  def inputIsValid(expenses: Seq[Expense]): Boolean = {
    allNamesUnique(expenses.map(_.person))
  }

  def allNamesUnique(names: Seq[Person]): Boolean = {
    names.length == names.distinct.length
  }

  def calcDebts(expenses: Seq[Expense]) = {
    val share: Amount = expenses.map(_.amount).sum / expenses.length
    val debtPerPerson: Seq[Debt] = expenses.map { expenseReport =>
      val Expense(person, amount) = expenseReport
      Debt(person, share - amount)
    }
    val (debtors, creditors) = debtPerPerson.partition(_.amount > 0)
    val debtsBySize = debtors.toSeq.sortBy(_.amount).reverse
    val creditsBySize = creditors.toSeq.sortBy(_.amount)
    val resolutions = resolve(
      debtsBySize=debtsBySize.toList,
      creditsBySize=creditsBySize.toList,
      resolutions=List.empty[DebtResolution])
    resolutions
  }

  def resolve(
    debtsBySize: List[Debt],
    creditsBySize: List[Debt],
    resolutions: List[DebtResolution]
  ): List[DebtResolution] = {
    if (debtsBySize.isEmpty || creditsBySize.isEmpty) {
      resolutions
    } else {
      val stepResolution= new ResolutionStep(
        debtsBySize=debtsBySize,
        creditsBySize=creditsBySize,
        resolutions=resolutions
        ).resolve
      resolve(
        debtsBySize=stepResolution.debtsBySize,
        creditsBySize=stepResolution.creditsBySize,
        resolutions=stepResolution.resolutions)
    }
  }
}


class ResolutionStep(
    debtsBySize: List[Debt],
    creditsBySize: List[Debt],
    resolutions: List[DebtResolution]
  ) {
  val Debt(debtor, debtAmount) :: debtsTail = debtsBySize
  val Debt(creditor, creditAmount) :: creditsTail = creditsBySize

  def resolve: StepResolution = {
    if (creditAmount == 0) {
      resolveDroppingCreditor
    } else if (debtAmount == 0) {
      resolveDroppingDebtor
    } else if (creditAmount.abs >= debtAmount.abs) {
      resolvePartOfCreditAmount
    } else {
      resolvePartOfDebtAmount
    }
  }

  private def resolveDroppingDebtor =
    StepResolution(
      debtsBySize=debtsTail,
      creditsBySize=creditsBySize,
      resolutions=resolutions)

  private def resolveDroppingCreditor =
    StepResolution(
      debtsBySize=debtsBySize,
      creditsBySize=creditsTail,
      resolutions=resolutions)

  private def resolvePartOfCreditAmount = {
    val creditorRemaining = creditAmount.abs - debtAmount.abs
    val creditorWithRemains = Debt(creditor, creditorRemaining)
    val resolution = DebtResolution(
      debtor=debtor,
      creditor=creditor,
      amount=debtAmount)
    StepResolution(
      debtsBySize=debtsTail,
      creditsBySize=(creditorWithRemains :: creditsTail),
      resolutions=(resolutions :+ resolution))
  }

  private def resolvePartOfDebtAmount = {
    val resolution = DebtResolution(
      debtor=debtor,
      creditor=creditor,
      amount=creditAmount.abs)
    val remainingDebt = debtAmount - creditAmount.abs
    val debtorWithRemains = Debt(debtor, remainingDebt)
    StepResolution(
      debtsBySize=(debtorWithRemains :: debtsTail),
      creditsBySize=creditsTail,
      resolutions=(resolutions :+ resolution))
  }
}


case class Expense(person: Person, amount: Amount)


case class Debt(person: Person, amount: Amount)


case class DebtResolution(debtor: Person, creditor: Person, amount: Amount) {
  override def toString = s"${debtor} should pay ${creditor} ${roundedAmount}"

  private def roundedAmount: BigDecimal =
    amount.setScale(2, RoundingMode.HALF_EVEN)
}


case class StepResolution(
    debtsBySize: List[Debt],
    creditsBySize: List[Debt],
    resolutions: List[DebtResolution])
