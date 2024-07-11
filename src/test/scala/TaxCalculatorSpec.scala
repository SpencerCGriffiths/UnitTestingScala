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


}
