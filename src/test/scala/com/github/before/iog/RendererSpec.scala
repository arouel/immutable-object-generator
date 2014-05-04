package com.github.before.iog

import org.specs2.mutable._
import Renderer._

class RendererSpec extends Specification {

  "Package" should {
    "validate that parts must not be empty" in {
      Package(Seq()) must throwA(new IllegalArgumentException("requirement failed: package must not be empty"))
    }
    "validates successfully if parts available" in {
      Package(Seq("org", "github", "before")) must equalTo(Package(Seq("org", "github", "before")))
    }
  }

  "TypeRef" should {
    "validate that package must not be empty" in {
      TypeRef(None, "Some") must throwA(new IllegalArgumentException("requirement failed: package must be defined"))
    }
    "validate that name must not be empty" in {
      TypeRef(Some(Package(Seq("org", "github"))), "") must throwA(new IllegalArgumentException("requirement failed: name must not be empty"))
    }
    "validates successfully" in {
      TypeRef(Some(Package(Seq("org", "github", "before"))), "Test") must equalTo(TypeRef(Some(Package(Seq("org", "github", "before"))), "Test"))
    }
    "validates successfully by using a fully qualified name" in {
      TypeRef.fullyQualifiedName("org.github.before.Test") must equalTo(TypeRef(Some(Package(Seq("org", "github", "before"))), "Test"))
    }
  }

  "Renderer" should {

    // imports
    val list = TypeRef.fullyQualifiedName("java.util.List");
    val long = TypeRef.fullyQualifiedName("java.lang.Long");
    val string = TypeRef.fullyQualifiedName("java.lang.String");

    // annotations
    val nonnull = Annotation(Package(Seq("javax", "annotation")), "Nonnull")
    val nullable = Annotation(Package(Seq("javax", "annotation")), "Nullable")
    val nonnegative = Annotation(Package(Seq("javax", "annotation")), "Nonnegative")

    // arguments
    val numberArg = Argument(Seq(), false, Int, "number")
    val textArg = Argument(Seq(nonnull), true, string, "text")
    val listArg = Argument(Seq(nonnull), true, list, "list")

    "render annotations" in {
      val pkg = Package(Seq("javax", "annotation"))
      render(Annotation(pkg, "Nullable")) must equalTo("@Nullable")
      render(Annotation(pkg, "Nonnull")) must equalTo("@Nonnull")
    }
    "render arguments" in {
      render(Argument(Seq(), false, Int, "number")) must equalTo("int number")
      render(Argument(Seq(), true, string, "text")) must equalTo("final String text")
      render(Argument(Seq(nonnull), false, string, "text")) must equalTo("@Nonnull String text")
      render(Argument(Seq(nonnull, nonnegative), true, long, "number")) must equalTo("@Nonnull @Nonnegative final Long number")
    }
    "render fields" in {
      render(Field(Seq(), Default, false, false, Char, "character", "'c'")) must equalTo("char character = 'c';")
      render(Field(Seq(), Private, false, true, Int, "number", null)) must equalTo("private final int number;")
      render(Field(Seq(), Private, true, true, Int, "number", null)) must equalTo("private static final int number;")
      render(Field(Seq(), Private, false, true, Int, "number", "1")) must equalTo("private final int number = 1;")
      render(Field(Seq(), Default, false, false, Void, "intoTheVoid", null)) must equalTo("Void intoTheVoid;")

      render(Field(Seq(nonnull), Public, false, false, string, "text", "\"some text\"")) must equalTo("@Nonnull\npublic String text = \"some text\";")

      val pattern = Annotation(Package(Seq("javax", "annotation")), "Pattern")
      render(Field(Seq(nonnull, pattern), Default, false, false, string, "text", "\"some text\"")) must equalTo("@Nonnull\n@Pattern\nString text = \"some text\";")
    }
    "render imports" in {
      render(TypeRef.fullyQualifiedName("javax.annotation.Nonnull")) must equalTo("import javax.annotation.Nonnull;")
    }
    "render methods" in {
      render(Method(Seq(), Default, false, false, string, Seq(), "getName", "return name;")) must equalTo("String getName() {\nreturn name;\n}")
      render(Method(Seq(nullable), Public, false, false, string, Seq(), "getName", "return name;")) must equalTo("@Nullable\npublic String getName() {\nreturn name;\n}")
      render(Method(Seq(), Public, true, false, string, Seq(numberArg), "convert", "return String.format(\"a number %s\", number);")) must equalTo("public static String convert(int number) {\nreturn String.format(\"a number %s\", number);\n}")
      render(Method(Seq(), Protected, false, true, list, Seq(numberArg, textArg, listArg), "doSomething", "return null;")) must equalTo("protected final List doSomething(int number, @Nonnull final String text, @Nonnull final List list) {\nreturn null;\n}")
    }
    "render package in compilation unit" in {
      render(CompilationUnit(None, Set(), Seq())) must equalTo("")

      val pkg = Package(Seq("org", "github", "before"))
      render(CompilationUnit(Some(pkg), Set(), Seq())) must equalTo("package org.github.before;")
    }
    "render primitives" in {
      render(Boolean) must equalTo("boolean")
      render(Byte) must equalTo("byte")
      render(Char) must equalTo("char")
      render(Double) must equalTo("double")
      render(Float) must equalTo("float")
      render(Int) must equalTo("int")
      render(Long) must equalTo("long")
      render(Short) must equalTo("short")
    }
    "render void" in {
      render(Void) must equalTo("void")
    }
    "render class with fields" in {
      val field1 = Field(Seq(), Private, true, true, Int, "myNumber1", "23")
      val field2 = Field(Seq(), Protected, false, false, Int, "myNumber2", "42")
      val clazz = Class(Public, false, "MyClass", Seq(field1, field2), Seq(), Seq())
      render(clazz) must contain("class MyClass")
      render(clazz) must contain("private static final int myNumber1 = 23;")
      render(clazz) must contain("protected int myNumber2 = 42;")
      render(clazz) must equalTo("""public class MyClass {

private static final int myNumber1 = 23;

protected int myNumber2 = 42;

}""")
    }
  }

}