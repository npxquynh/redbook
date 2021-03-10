import org.specs2.mutable.Specification

class SimpleSpec extends Specification {

  "1+1 should be 2" in {
    1 + 1 must equalTo(2)
  }

}
