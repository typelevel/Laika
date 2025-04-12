package laika.api.config

import laika.api.config.ConfigError.{ DecodingFailed, ResolverFailed }
import laika.api.config.ConfigValue.{ LongValue, ObjectValue }
import munit.FunSuite

class ConfigBuilderSpec extends FunSuite {

  def getLeftValue: Either[ConfigError, Int] =
    Left(DecodingFailed("expected error"))

  test("ConfigValue.delay") {
    val config   = ConfigBuilder.empty
      .withValue("foo.bar", 7)
      .withValue("foo.baz", ConfigValue.delay(9))
      .build
    val res      = config.get[ConfigValue]("foo")
    val expected = ObjectValue(
      Seq(
        Field("bar", LongValue(7)),
        Field("baz", LongValue(9))
      )
    )
    assertEquals(res, Right(expected))
  }

  test("ConfigValue.eval - Right") {
    val config   = ConfigBuilder.empty
      .withValue("foo.bar", 7)
      .withValue("foo.baz", ConfigValue.eval(Right(9)))
      .build
    val res      = config.get[ConfigValue]("foo")
    val expected = ObjectValue(
      Seq(
        Field("bar", LongValue(7)),
        Field("baz", LongValue(9))
      )
    )
    assertEquals(res, Right(expected))
  }

  test("ConfigValue.eval - Left") {
    val config   = ConfigBuilder.empty
      .withValue("foo.bar", 7)
      .withValue("foo.baz", ConfigValue.eval(getLeftValue))
      .build
    val res      = config.get[ConfigValue]("foo")
    val expected =
      ResolverFailed("One or more errors resolving fields of object value: expected error")
    assertEquals(res, Left(expected))
  }

  test("ConfigValue.eval with Config - Right") {
    val dependentValue = ConfigValue.eval { config =>
      config.get[Int]("ref").map(_ + 5)
    }
    val config         = ConfigBuilder.empty
      .withValue("ref", 10)
      .withValue("foo.bar", 7)
      .withValue("foo.baz", dependentValue)
      .build
    val res            = config.get[ConfigValue]("foo")
    val expected       = ObjectValue(
      Seq(
        Field("bar", LongValue(7)),
        Field("baz", LongValue(15))
      )
    )
    assertEquals(res, Right(expected))
  }

  test("ConfigValue.eval with Config - Left") {
    val dependentValue = ConfigValue.eval { config =>
      config.get[Int]("ref")
    }
    val config         = ConfigBuilder.empty
      .withValue("foo.bar", 7)
      .withValue("foo.baz", dependentValue)
      .build
    val res            = config.get[ConfigValue]("foo")
    val expected       = ResolverFailed(
      "One or more errors resolving fields of object value: Invalid Field 'baz': Not found: 'ref'"
    )
    assertEquals(res, Left(expected))
  }

}
