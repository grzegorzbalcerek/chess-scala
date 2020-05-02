package chess

import zio.test.Assertion._
import zio.test._

object FieldTest extends DefaultRunnableSpec {

  def spec = suite("field test")(
    test("toString"){
      assert(Field(1,1).toString)(equalTo("a1")) &&
      assert(Field(2,1).toString)(equalTo("b1")) &&
      assert(Field(2,3).toString)(equalTo("b3")) &&
      assert(Field(8,8).toString)(equalTo("h8"))
    },
    test("relative"){
      assert(Field(2,3).relative(1,1).toString)(equalTo("c4")) &&
        assert(Field(2,3).relative(4,5).toString)(equalTo("f8"))
    },
    test("isLastRow"){
      assert(Field(8,8).isLastRow(White))(isTrue) &&
      assert(Field(7,8).isLastRow(White))(isTrue) &&
      assert(Field(7,8).isLastRow(Black))(isFalse) &&
      assert(Field(7,1).isLastRow(Black))(isTrue) &&
      assert(Field(2,2).isLastRow(White))(isFalse)
    },
    test("isValid"){
      assert(Field(2,2).isValid)(isTrue) &&
        assert(Field(0,2).isValid)(isFalse) &&
        assert(Field(2,0).isValid)(isFalse) &&
        assert(Field(2,9).isValid)(isFalse) &&
        assert(Field(9,2).isValid)(isFalse)
    }
  )
}
