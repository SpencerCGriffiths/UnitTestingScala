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
    if (income > 50000 && income <= 125000) true else false
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isAdditionalRateTaxpayer(income: Double): Boolean = {
    if (income > 125000) true else false
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if (income <= 10000) {
      "£10,000"
    } else if (income <= 50000) {
      "£50,000"
    } else if (income <= 125000) {
      "£125,000"
    } else if (income > 125000) {
      "No Limit"
    } else {
      "404"
    }
  }

  /** Capital Gains tax */

  def capitalGainsCalculator(residentialProperty: Boolean, income: Double, gainYTD: Double): Double = {

    // Could extend to factor in all the real life conditions!

    // Capital Gain Tax Rates for all but property
    val brCPTexResiden: Double = 0.10
    val hrCPTexResiden: Double = 0.20

    // Capital Gain Tax Rate for property
    val brCPTResiden: Double = 0.18
    val hrCPTResiden: Double = 0.24

    //Tax Free allowance
    val taxFreeAllowance: Double = 3000

    // Utility functions:
    def isBRCapitalGains(income: Double): Boolean = {
      if (income <= basicRateLimit) true else false
    }

    def brCapitalGainsProperty(gainYTD: Double): Double = {
      gainYTD match {
        case gainYTD if gainYTD < taxFreeAllowance => 0
        case gainYTD if gainYTD >= taxFreeAllowance => (gainYTD - taxFreeAllowance) * brCPTResiden
        case _ => 404
      }
    }

    def hrCapitalGainsProperty(gainYTD: Double): Double = {
      gainYTD match {
        case gainYTD if gainYTD < taxFreeAllowance => 0
        case gainYTD if gainYTD >= taxFreeAllowance => (gainYTD - taxFreeAllowance) * hrCPTResiden
        case _ => 404
      }
    }

    def brCapitalGains(gainYTD: Double): Double = {
      gainYTD match {
        case gainYTD if gainYTD < taxFreeAllowance => 0
        case gainYTD if gainYTD >= taxFreeAllowance => (gainYTD - taxFreeAllowance) * brCPTexResiden
        case _ => 404
      }
    }

    def hrCapitalGains(gainYTD: Double): Double = {
      gainYTD match {
        case gainYTD if gainYTD < taxFreeAllowance => 0
        case gainYTD if gainYTD >= taxFreeAllowance => (gainYTD - taxFreeAllowance) * hrCPTexResiden
        case _ => 404
      }
    }

    residentialProperty match {
      case residentialProperty if residentialProperty && isBRCapitalGains(income) => brCapitalGainsProperty(gainYTD)
      case residentialProperty if residentialProperty && !isBRCapitalGains(income) => hrCapitalGainsProperty(gainYTD)
      case residentialProperty if !residentialProperty && isBRCapitalGains(income) => brCapitalGains(gainYTD)
      case residentialProperty if !residentialProperty && !isBRCapitalGains(income) => hrCapitalGains(gainYTD)
      case _ => 404
    }

  }

  /** Calculate both Tax */

  def incomeAndCapitalGains (income: Double, soldAssetsOrShares: Boolean = false, residentialProperty: Boolean = false, gainYTD: Double = 0): Double = {

   if(!soldAssetsOrShares) {
     calculateTax(income)
   } else if (soldAssetsOrShares) {
     calculateTax(income) + capitalGainsCalculator(residentialProperty, income, gainYTD)
   } else {
     404
   }
  }
}

// Basic Capital Gains Calculator:
//Define a method- take in isResidentialProperty:Bool, Income:Double, gainYearToDate:Double
// Set 4 vals - BR Exclude Res, BR Res etc...
// if income is higher rater or additional (use method) && is Residential && gain is over 3

// A method which can tell you if someone is a higher rate taxpayer



// Do not pay capital Gains Tax on:
//private motor cars, including vintage cars
//gifts to UK registered charities
//some government securities
//prizes and betting winnings
//cash
//stocks and shares held in an ISA
//foreign currency held for your own use

// Property
// Main residence relief- if it is your only or main home no CGT.
// apply to houseboat
// apply to residential caravans
// apply to garden area up to half a hectare as part of home (or larger if required)
// married couple can have a seperate house if it is going to be permanent
// you can nominate a property if it is deemed to be a residence (full time or not)
// nomination should be made within 2 years of a change in the combination of residence
