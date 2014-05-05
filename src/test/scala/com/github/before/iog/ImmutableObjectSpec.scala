package com.github.before.iog

import org.specs2.mutable._
import ImmutableObject._

class ImmutableObjectSpec extends Specification {

  "ImmutableObject" should {

    val pkg = Package(Seq("org", "github", "before", "test"))
    val stringType = TypeRef.fullyQualifiedName("java.lang.String")
    val listOfStringsType = TypeRef.fullyQualifiedName("java.util.List", Seq(GenericType(stringType)))
    val imports = Set(listOfStringsType)
    val numberField = Field(Seq(), Private, false, true, Int, "number")
    val listField = Field(Seq(), Private, false, true, listOfStringsType, "strings")
    val fields = Seq(numberField, listField)
    val numberMethod = Method(Seq(), Public, false, false, Int, Seq(), "getNumber", "return this." + numberField.name + ";")
    val listMethod = Method(Seq(), Public, false, false, listOfStringsType, Seq(), "getStrings", "return this." + listField.name + ";")
    val methods = Seq(numberMethod, listMethod)
    val className = "TestImmutable"
    val clazz = Class(Public, true, className, fields, Seq(), methods, Seq(), Some(pkg))
    val types = Seq(clazz)
    val compilationUnit = CompilationUnit(Some(pkg), imports, types)

    "define an immutable object" in {
      val number = Member("number", Int)
      val strings = Member("strings", TypeRef.fullyQualifiedName("java.util.List", Seq(GenericType(stringType))))
      val members = Seq(number, strings)
      define(pkg, className, members) must equalTo(CompilationUnit(Some(pkg), imports, types))
    }

  }

}