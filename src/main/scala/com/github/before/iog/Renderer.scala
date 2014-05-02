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

    // everything else
    case Annotation(pkg, name) => "@" + name
    case Argument(annotations, finalModifier, t, name) =>
      renderAnnotations(annotations, " ") +
        renderFinalModifier(finalModifier) +
        renderFieldType(t) +
        renderName(name)
    case Class(
      accessModifier,
      finalModifier,
      name,
      fields,
      methods,
      types) => renderAccessModifier(accessModifier) + renderFinalModifier(finalModifier) + s"class $name {\n\n" + renderWithSeparator(fields, "\n\n") + "\n\n}"
    case CompilationUnit(pkg, imports, types) => render(pkg) + "\n\n" + renderWithSeparator(imports, "\n") + "\n\n" + renderWithSeparator(types, "\n")
    case Field(
      annotations,
      accessModifier,
      staticModifier,
      finalModifier,
      t,
      name,
      value
      ) =>
      renderAnnotations(annotations) + renderAccessModifier(accessModifier) + renderStaticModifier(staticModifier) + renderFinalModifier(finalModifier) + renderFieldType(t) + " " + name + renderFieldValue(value) + ";"
    case Method(annotations, accessModifier, staticModifier, finalModifier, returnType, args, name, body) =>
      renderAnnotations(annotations) +
        renderAccessModifier(accessModifier) +
        renderStaticModifier(staticModifier) +
        renderFinalModifier(finalModifier) +
        renderFieldType(returnType) +
        renderName(name) +
        renderArguments(args) +
        renderMethodBody(body)
    case Import(pkg, name) => "import " + pkg.parts.mkString(".") + "." + name + ";"
    case Package(ps) => if (ps.isEmpty) "" else "package " + ps.mkString(".")
  }

  private def renderAccessModifier(accessModifier: AccessModifier): String = if (accessModifier == Default) "" else render(accessModifier) + " "
  private def renderAnnotations(annotations: Seq[Annotation], separator: String = "\n"): String = if (annotations.isEmpty) "" else renderWithSeparator(annotations, separator) + separator
  private def renderArguments(arguments: Seq[Argument]): String = if (arguments.isEmpty) "() " else "(" + renderWithSeparator(arguments, ", ") + ") "
  private def renderFieldValue(value: String): String = if (value == null) "" else " = " + value
  private def renderFinalModifier(finalModifier: Boolean): String = if (finalModifier) "final " else ""
  private def renderFieldType(t: Type): String = t match {
    case p: Primitive => render(p)
    case Void => "Void"
    case Import(pkg, name) => name
    case Class(
      accessModifier,
      finalModifier,
      name,
      fields,
      methods,
      types) => name
  }
  private def renderMethodBody(body: String): String = s"{\n$body\n}"
  private def renderName(name: String): String = " " + name
  private def renderStaticModifier(staticModifier: Boolean): String = if (staticModifier) "static " else ""
  private def renderWithSeparator[T <: Renderable](elements: Iterable[T], separator: String): String = elements.map(i => render(i)).mkString(separator)

}

sealed trait Renderable

case class Annotation(
  val pkg: Package,
  val name: String)
  extends Renderable

/**
 * A data type
 */
sealed trait Type extends Renderable

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
  val accessModifier: AccessModifier,
  val staticModifier: Boolean,
  val finalModifier: Boolean,
  val `type`: Type,
  val name: String,
  val value: String)
  extends Renderable

case class Argument(
  val annotations: Seq[Annotation],
  val finalModifier: Boolean,
  val `type`: Type,
  val name: String)
  extends Renderable

case class Method(
  val annotations: Seq[Annotation],
  val accessModifier: AccessModifier,
  val staticModifier: Boolean,
  val finalModifier: Boolean,
  val returnType: Type,
  val args: Seq[Argument],
  val name: String,
  val body: String)
  extends Renderable
