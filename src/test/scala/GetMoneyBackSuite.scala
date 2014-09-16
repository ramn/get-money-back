package se.ramn.getmoneyback

import org.scalatest.FunSuite


class GetMoneyBackSuite extends FunSuite {
  test("parsing inputs") {
    val result = GetMoneyBack.parseInputs(
      List(
        "ada 123.12",
        "kolle 887.21"))
    assert(result === List(
      Expense("ada", BigDecimal("123.12")),
      Expense("kolle", BigDecimal("887.21"))))
  }

  test("validates uniqueness of names") {
    val result = GetMoneyBack.inputIsValid(
      List(
        Expense("Ada", BigDecimal(1.1)),
        Expense("Ada", BigDecimal(1.1))))
    assert(result === false, "should be false - names are not unique")
  }

  test("calcDebts") {
    val expenses = GetMoneyBack.parseInputs(
      List(
        "ada 200",
        "kolle 100",
        "leila 20",
        "billy 10"
      ))
    val result = GetMoneyBack.calcDebts(expenses)
    assert(result === List(
      DebtResolution("billy", "ada", BigDecimal("72.5")),
      DebtResolution("leila", "ada", BigDecimal("45")),
      DebtResolution("leila", "kolle", BigDecimal("17.5"))
      ))
  }
}
