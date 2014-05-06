package com.github.before.iog

import org.specs2.mutable._
import ImmutableObject._

class ImmutableObjectSpec extends Specification {

  def fieldToArg(field: Field): Argument = Argument(Seq(), true, field.`type`, field.name)
  def fieldToAssignment(field: Field): String = "this." + field.name + " = " + field.name + ";"
  def fieldsToArgs(fields: Seq[Field]): Seq[Argument] = fields.map(fieldToArg(_))
  def fieldsToAssignments(fields: Seq[Field]): String = fields.map(fieldToAssignment(_)).mkString("\n")

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
    val className = "TestImmutable"
    val methods = Seq(equalsMethod(className, fields), numberMethod, listMethod)
    val constr = Constructor(Public, fieldsToArgs(fields), className, fieldsToAssignments(fields))
    val immutable = Class(Public, true, className, fields, Seq(constr), methods, Seq(), Some(pkg))
    val types = Seq(immutable)
    val compilationUnit = CompilationUnit(Some(pkg), imports, types)

    "define an immutable object" in {
      val number = Member("number", Int)
      val strings = Member("strings", TypeRef.fullyQualifiedName("java.util.List", Seq(GenericType(stringType))))
      val members = Seq(number, strings)
      Renderer.render(define(pkg, className, members)) must equalTo(Renderer.render(CompilationUnit(Some(pkg), imports, types)))
      define(pkg, className, members) must equalTo(CompilationUnit(Some(pkg), imports, types))
    }

    "equalsMethod of an immutable object" in {
      val overrideAnnotation = Annotation(Package(Seq("java", "lang")), "Override")
      val objArg = Argument(Seq(), true, TypeRef.fullyQualifiedName("java.lang.Object"), "obj")
      val body = s"""if (this == obj) return true;
if (obj == null) return false;
if (getClass() != obj.getClass()) return false;
$className other = ($className) obj;
if (number != other.number) return false;
if (strings == null) {
if (other.strings != null) return false;
} else if (!strings.equals(other.strings)) return false;
return true;"""
      equalsMethod(className, fields) must equalTo(Method(Seq(overrideAnnotation), Public, false, false, Boolean, Seq(objArg), "equals", body))
    }

  }

}