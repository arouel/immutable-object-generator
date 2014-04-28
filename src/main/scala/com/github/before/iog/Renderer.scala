package com.github.before.iog

object Renderer {
  def render(node: Renderable): String = node match {
    // primitive types
    case Boolean => "boolean"
    case Byte => "byte"
    case Char => "char"
    case Double => "double"
    case Float => "float"
    case Int => "int"
    case Long => "long"
    case Short => "short"
    case Void => "void"

    // access modifiers
    case Default => ""
    case Private => "private"
    case Protected => "protected"
    case Public => "public"

    // everthing else
    case Annotation(pkg, name) => "@" + name
    case Class(
      accessModifier,
      finalModifier,
      name,
      fields,
      methods,
      types) => renderAccessModifier(accessModifier) + renderFinalModifier(finalModifier) + s"class $name {\n}"
    case CompilationUnit(pkg, is, types) => render(pkg) + "\n\n" + is.map(i => render(i)).mkString("\n") + "\n\n" + types.map(t => render(t)).mkString("\n")
    case Import(pkg, name) => "import " + pkg.parts.mkString(".") + "." + name + ";"
    case Package(ps) => if (ps.isEmpty) "" else "package " + ps.mkString(".")
  }

  private def renderAccessModifier(accessModifier: AccessModifier): String = if (accessModifier == Default) "" else render(accessModifier) + " "
  private def renderFinalModifier(finalModifier: Boolean): String = if (finalModifier) "final " else ""

}

sealed trait Renderable

case class Annotation(
  val pkg: Package,
  val name: String)
  extends Type with Renderable

/**
 * A data type
 */
trait Type

/**
 * A simple data type is a primitive type or void.
 */
sealed trait SimpleType extends Type

/**
 * A reference type is a data type that’s based on a class rather than on one of the primitive types that are built in to the Java language.
 */
sealed trait ReferenceType extends Type with Renderable {
  def name: String
}

case class CompilationUnit(val pkg: Package, val imports: Set[Import], val types: Seq[ReferenceType]) extends Renderable

case class Package(val parts: Seq[String]) extends Renderable

case class Import(
  val pkg: Package,
  val name: String)
  extends Type with Renderable {
  require(!pkg.parts.isEmpty, "package must not be empty")
  require(!name.isEmpty, "name must not be empty")
}

object Import {
  def fullyQualifiedName(fqn: String): Import = {
    val parts = fqn.split('.')
    val pkg = Package(parts.dropRight(1))
    val name = parts.last
    Import(pkg, name)
  }
}

/**
 * Primitive data types are defined by the language itself.
 */
sealed abstract class Primitive extends SimpleType with Renderable
case object Boolean extends Primitive
case object Byte extends Primitive
case object Char extends Primitive
case object Double extends Primitive
case object Float extends Primitive
case object Long extends Primitive
case object Int extends Primitive
case object Short extends Primitive

/**
 * Java Access Modifiers public, private and protected
 */
sealed abstract class AccessModifier extends Renderable
case object Default extends AccessModifier
case object Public extends AccessModifier
case object Private extends AccessModifier
case object Protected extends AccessModifier

/**
 * Void is not a type and means *nothing*
 */
case object Void extends SimpleType with Renderable

case class Class(
  val accessModifier: AccessModifier,
  val finalModifier: Boolean,
  override val name: String,
  val fields: Seq[Field],
  val methods: Seq[Method],
  val types: Seq[ReferenceType])
  extends ReferenceType

case class Field(
  val annotations: Seq[Annotation],
  val staticModifier: Boolean,
  val finalModifier: Boolean,
  val `type`: Type,
  val name: String,
  val value: String)

case class Argument(
  val annotations: Seq[Annotation],
  val finalModifier: Boolean,
  val `type`: Type,
  val name: String)

case class Method(
  val annotations: Seq[Annotation],
  val staticModifier: Boolean,
  val finalModifier: Boolean,
  val returnType: Type,
  val args: Seq[Argument],
  val name: String,
  val body: String)
