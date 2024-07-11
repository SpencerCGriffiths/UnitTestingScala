import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  "TaxCalculator.calculateTax" should {

    "return the total amount of tax to pay" when {

      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(10000)
        assert(result == 0)
      }

      "the income is below the basic rate limit but above the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(50000)
        assert(result == 8000)
      }

      "the income is below the higher Rater limit but above the basic rate " in {
        val result: Double = taxCalculator.calculateTax(125000)
        assert(result == 38000)
      }

      "the income is above the higher rate" in {
        val result: Double = taxCalculator.calculateTax(150000)
        assert(result == 49250)
      }
    }
  }

  "TaxCalculator.isHigherRateTaxpayer" should {
    "return true" when {
      "the income is above (not inclusive of) the basic Rate limit of 50000" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(50001)
        assert(result)
      }
    }
    "return true" when {
      "the income is below (inclusive of) the higher Rate limit of 125000" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125000)
        assert(result)
      }
    }
    "return false" when {
      "the income is below (inclusive of) the  basic Rate limit 50000" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(50000)
        assert(!result)
      }
    }
    "return false" when {
      "the income is above (not inclusive of) the higher Rate Limitbasic 125000" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125001)
        assert(!result)
      }
    }
  }

  "TaxCalculator.isAdditionalRateTaxpayer" should {
    "return true" when {
      "the income is above (not inclusive of) the higher Rater limit 125000" in {
        val result: Boolean = taxCalculator.isAdditionalRateTaxpayer(125001)
        assert(result)
      }
    }
    "return false" when {
      "the income is below (inclusive of) the  higher Rate limit 125000" in {
        val result: Boolean = taxCalculator.isAdditionalRateTaxpayer(125000)
        assert(!result)
      }
    }
  }

  "TaxCalculator.formattedCurrentTaxAllowance" should {

    "Return a formatted string of the upper limit of the users current tax band" when {

      "the income is lower than the personal allowance of £10,000 (inclusive of)" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(10000)
        assert(result == "£10,000")

        val result1: String = taxCalculator.formattedCurrentTaxAllowance(0)
        assert(result1 == "£10,000")

        val result2: String = taxCalculator.formattedCurrentTaxAllowance(9999)
        assert(result2 == "£10,000")
      }

    "the income is above the personal allowance of £10,000 (not inclusive of) but lower than the basic rate limit of £50,000 (inclusive of)" in {
      val result: String = taxCalculator.formattedCurrentTaxAllowance(10001)
      assert(result == "£50,000")

      val result1: String = taxCalculator.formattedCurrentTaxAllowance(49999)
      assert(result1 == "£50,000")

      val result2: String = taxCalculator.formattedCurrentTaxAllowance(50000)
      assert(result2 == "£50,000")
      }

    "the income is above the basic rate allowance of £50,000 (not inclusive of) but lower than the higher rate limit of £125,000 (inclusive of)" in {
      val result: String = taxCalculator.formattedCurrentTaxAllowance(50001)
      assert(result == "£125,000")

      val result1: String = taxCalculator.formattedCurrentTaxAllowance(124999)
      assert(result1 == "£125,000")

      val result2: String = taxCalculator.formattedCurrentTaxAllowance(125000)
      assert(result2 == "£125,000")
      }

    "the income is above the higher rate limit of £125,000 (not inclusive of)" in {
      val result: String = taxCalculator.formattedCurrentTaxAllowance(125001)
      assert(result == "No Limit")

      val result1: String = taxCalculator.formattedCurrentTaxAllowance(150000)
      assert(result1 == "No Limit")

      // Could also write tests to the opposite for all categories
      val result2: String = taxCalculator.formattedCurrentTaxAllowance(125000)
      assert(result2 != "No Limit")
      }

    }

  }

  "TaxCalculator.capitalGainsCalculator" should {

    "Calculate the correct Capital Gains Tax for property" when {

      "The income is lower than basic rate, gains less than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 40000, gainYTD = 2000)
        assert(result == 0)
      }

      "The income is lower than basic rate, gains more than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 40000, gainYTD = 5000)
        assert(result == 360)
      }

      "The income is the basic rate,gains less than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 50000, gainYTD = 2000)
        assert(result == 0)
      }

      "The income is the basic rate, gains more than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 50000, gainYTD = 5000)
        assert(result == 360)
      }

      "The income is above the basic rate, gains less than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 55000, gainYTD = 2000)
        assert(result == 0)
      }
      "The income is above the basic rate, gains more than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = true, income = 55000, gainYTD = 5000)
        assert(result == 480)
      }
    }

    "Calculate the correct Capital Gains Tax for anything other than property" when {

      "The income is lower than basic rate, gains less than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 40000, gainYTD = 2000)
        assert(result == 0)
      }

      "The income is lower than basic rate, gains more than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 40000, gainYTD = 5000)
        assert(result == 200)
      }

      "The income is the basic rate,gains less than the taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 50000, gainYTD = 2000)
        assert(result == 0)
      }

      "The income is the basic rate, gains more than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 50000, gainYTD = 5000)
        assert(result == 200)
      }

      "The income is above the basic rate, gains less than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 55000, gainYTD = 2000)
        assert(result == 0)
      }
      "The income is above the basic rate, gains more than taxable amount" in {
        val result: Double = taxCalculator.capitalGainsCalculator(residentialProperty = false, income = 55000, gainYTD = 5000)
        assert(result == 400)
      }
    }
  }

  "TaxCalculator.incomeAndCapitalGains" should {

    "return the total amount of tax owed when shares or assets have NOT been sold" when {

      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(income = 10000)
        assert(result == 0)
      }

      "the income is below the basic rate limit but above the personal tax limit" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(income = 50000)
        assert(result == 8000)
      }

      "the income is below the higher Rater limit but above the basic rate " in {
        val result: Double = taxCalculator.incomeAndCapitalGains(income = 125000)
        assert(result == 38000)
      }

      "the income is above the higher rate" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(income = 150000)
        assert(result == 49250)
      }
    }

    "return the total amount of tax owed when PROPERTY has been sold" when {

        "The income is lower than personal allowance limit, gains less than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 10000, gainYTD = 2000, soldAssetsOrShares = true)
          assert(result == 0)
        }

        "The income is lower than personal allowance limit, gains more than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 10000, gainYTD = 5000, soldAssetsOrShares = true)
          assert(result == 360)
        }

        "The income is over the personal allowance but lower than basic rate limit, gains less than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 50000, gainYTD = 2000, soldAssetsOrShares = true)
          assert(result == 8000)
        }

        "The income is over the personal allowance but lower than basic rate limit, gains more than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 50000, gainYTD = 5000, soldAssetsOrShares = true)
          assert(result == 8360)
        }

        "The income is over basic rate but lower than the higher rate limit, gains less than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 125000, gainYTD = 2000, soldAssetsOrShares = true)
          assert(result == 38000)
        }

        "The income is over basic rate but lower than the higher rate limit, gains more than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 125000, gainYTD = 5000, soldAssetsOrShares = true)
          assert(result == 38480)
        }

        "The income is over the higher limit, gains less than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 150000, gainYTD = 2000, soldAssetsOrShares = true)
          assert(result == 49250)
        }

        "The income is over the higher limit, gains more than the taxable amount" in {
          val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = true, income = 150000, gainYTD = 5000, soldAssetsOrShares = true)
          assert(result == 49730)
        }
    }


    "return the total amount of tax owed when OTHER SHARES AND ASSESTS have been sold" when {

      "The income is lower than personal allowance limit, gains less than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 10000, gainYTD = 2000, soldAssetsOrShares = true)
        assert(result == 0)
      }

      "The income is lower than personal allowance limit, gains more than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 10000, gainYTD = 5000, soldAssetsOrShares = true)
        assert(result == 200)
      }

      "The income is over the personal allowance but lower than basic rate limit, gains less than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 50000, gainYTD = 2000, soldAssetsOrShares = true)
        assert(result == 8000)
      }

      "The income is over the personal allowance but lower than basic rate limit, gains more than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 50000, gainYTD = 5000, soldAssetsOrShares = true)
        assert(result == 8200)
      }

      "The income is over basic rate but lower than the higher rate limit, gains less than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 125000, gainYTD = 2000, soldAssetsOrShares = true)
        assert(result == 38000)
      }

      "The income is over basic rate but lower than the higher rate limit, gains more than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 125000, gainYTD = 5000, soldAssetsOrShares = true)
        assert(result == 38400)
      }

      "The income is over the higher limit, gains less than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 150000, gainYTD = 2000, soldAssetsOrShares = true)
        assert(result == 49250)
      }

      "The income is over the higher limit, gains more than the taxable amount" in {
        val result: Double = taxCalculator.incomeAndCapitalGains(residentialProperty = false, income = 150000, gainYTD = 5000, soldAssetsOrShares = true)
        assert(result == 496250)
      }
    }

  }

}