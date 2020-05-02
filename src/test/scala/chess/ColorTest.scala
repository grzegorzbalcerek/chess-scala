package chess

import zio.test._
import Assertion._

object ColorTest extends DefaultRunnableSpec {

  def spec = suite("color test")(
    test("other"){
      assert(White.other)(equalTo(Black)) &&
        assert(Black.other)(equalTo(White))
    },
    test("firstRow"){
      assert(White.firstRow)(equalTo(1)) &&
        assert(Black.firstRow)(equalTo(8))
    }
  )
}
