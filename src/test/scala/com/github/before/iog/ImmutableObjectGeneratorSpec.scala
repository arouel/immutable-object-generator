package com.github.before.iog

import org.specs2.mutable._
import ImmutableObjectGenerator._

class ImmutableObjectGeneratorSpec extends Specification {

  "The generator" should {

    val pkg = Package(Seq("org", "github", "before", "test"))
    val listType = Import.fullyQualifiedName("java.util.List")
    val imports = Set(listType)
    val fields = Seq(Field(Seq(), Private, false, true, listType, "myList", null))
    val methods = Seq(Method(Seq(), false, true, listType, Seq(), "getMyList", null))
    val clazz = Class(Public, true, "MyTestClass", fields, methods, Seq())
    val types = Seq(clazz)
    val compilationUnit = CompilationUnit(pkg, imports, types)

    "starts with package" in {
      generate(compilationUnit, Settings()) must startWith("package")
    }

    "ends with }" in {
      generate(compilationUnit, Settings()) must endWith("}")
    }

    "contains class name" in {
      generate(compilationUnit, Settings()) must contain("public final class MyTestClass ")
    }
  }

}