package com.github.before.iog

import org.specs2.mutable._
import Renderer._

class RendererSpec extends Specification {

  "Import" should {
    "validate that package must not be empty" in {
      Import(Package(Seq()), "some") must throwA(new IllegalArgumentException("requirement failed: package must not be empty"))
    }
    "validate that name must not be empty" in {
      Import(Package(Seq("org", "github")), "") must throwA(new IllegalArgumentException("requirement failed: name must not be empty"))
    }
    "validates successfully" in {
      Import(Package(Seq("org", "github", "before")), "Test") must equalTo(Import(Package(Seq("org", "github", "before")), "Test"))
    }
    "validates successfully by using a fully qualified name" in {
      Import.fullyQualifiedName("org.github.before.Test") must equalTo(Import(Package(Seq("org", "github", "before")), "Test"))
    }
  }

  "Renderer" should {
    "render annotations" in {
      render(Annotation(Package(Seq()), "Nullable")) must equalTo("@Nullable")
      render(Annotation(Package(Seq("javax", "annotation")), "Nonnull")) must equalTo("@Nonnull")
    }
    "render fields" in {
      render(Field(Seq(), Default, false, false, Char, "character", "'c'")) must equalTo("char character = 'c';")
      render(Field(Seq(), Private, false, true, Int, "number", null)) must equalTo("private final int number;")
      render(Field(Seq(), Private, true, true, Int, "number", null)) must equalTo("private static final int number;")
      render(Field(Seq(), Private, false, true, Int, "number", "1")) must equalTo("private final int number = 1;")
      render(Field(Seq(), Default, false, false, Void, "intoTheVoid", null)) must equalTo("Void intoTheVoid;")

      val string = Import.fullyQualifiedName("java.lang.String");
      val nonnull = Annotation(Package(Seq("javax", "annotation")), "Nonnull")
      render(Field(Seq(nonnull), Public, false, false, string, "text", "\"some text\"")) must equalTo("@Nonnull\npublic String text = \"some text\";")

      val pattern = Annotation(Package(Seq("javax", "annotation")), "Pattern")
      render(Field(Seq(nonnull, pattern), Default, false, false, string, "text", "\"some text\"")) must equalTo("@Nonnull\n@Pattern\nString text = \"some text\";")
    }
    "render imports" in {
      render(Import(Package(Seq("javax", "annotation")), "Nonnull")) must equalTo("import javax.annotation.Nonnull;")
    }
    "render packages" in {
      render(Package(Seq())) must equalTo("")
      render(Package(Seq("javax", "annotation"))) must equalTo("package javax.annotation")
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
      val field2 = Field(Seq(), Private, true, true, Int, "myNumber2", "24")
      val clazz = Class(Public, false, "MyClass", Seq(field1, field2), Seq(), Seq())
      render(clazz) must equalTo("""public class MyClass {

private static final int myNumber1 = 23;

private static final int myNumber2 = 24;

}""")
    }
  }

}