class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    val taxWhenBasicRateReached: Double = (basicRateLimit - personalAllowance) * basicRate
    val taxWhenHigherLimitReached: Double = ((higherRateLimit - basicRateLimit) * higherRate) + taxWhenBasicRateReached

    if (income <= 10000) {
      0
    } else if (income <= 50000) {
      val basicRateTaxableIncome = income - personalAllowance
      val taxToPay = basicRateTaxableIncome * basicRate
      taxToPay
    } else if (income <= 125000) {
      val higherRateTaxableIncome = income - basicRateLimit
      val taxToPay = (higherRateTaxableIncome * higherRate) + taxWhenBasicRateReached
      taxToPay
    } else if (income > 125000) {
      val additionalTaxableIncome = income - higherRateLimit
      val taxToPay = (additionalTaxableIncome * additionalRate) + taxWhenHigherLimitReached
      taxToPay
    } else {
      404
    }
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    if(income > 50000 && income <= 125000) true else false
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if (income <= 10000) {
      "£10,000"
    } else if (income <= 50000) {
      "50,000"
    } else if (income <= 125000) {
      "125000"
    } else if (income > 125000) {
      "No Limit"
    } else {
      "404"
    }
  }

}
