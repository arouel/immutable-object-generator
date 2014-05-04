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
      types,
      pkg,
      generics) =>
      renderAccessModifier(accessModifier) +
        renderFinalModifier(finalModifier) +
        "class " + name + " {" +
        renderSection(fields, "\n\n") +
        "\n\n}"
    case CompilationUnit(pkg, imports, types) =>
      renderPackageDeclaration(pkg) +
        renderSection(imports) +
        renderSection(types)
    case Field(
      annotations,
      accessModifier,
      staticModifier,
      finalModifier,
      t,
      name,
      value
      ) =>
      renderAnnotations(annotations) +
        renderAccessModifier(accessModifier) +
        renderStaticModifier(staticModifier) +
        renderFinalModifier(finalModifier) +
        renderFieldType(t) + " " + name +
        renderFieldValue(value) + ";"
    case Method(annotations, accessModifier, staticModifier, finalModifier, returnType, args, name, body) =>
      renderAnnotations(annotations) +
        renderAccessModifier(accessModifier) +
        renderStaticModifier(staticModifier) +
        renderFinalModifier(finalModifier) +
        renderFieldType(returnType) +
        renderName(name) +
        renderArguments(args) +
        renderMethodBody(body)
    case TypeRef(pkg, name, _) => if (pkg.isEmpty) "" else "import " + renderPackage(pkg.get) + "." + name + ";"
  }

  private def renderAccessModifier(accessModifier: AccessModifier): String = if (accessModifier == Default) "" else render(accessModifier) + " "
  private def renderAnnotations(annotations: Seq[Annotation], separator: String = "\n"): String = if (annotations.isEmpty) "" else renderWithSeparator(annotations, separator) + separator
  private def renderArguments(arguments: Seq[Argument]): String = if (arguments.isEmpty) "() " else "(" + renderWithSeparator(arguments, ", ") + ") "
  private def renderFieldValue(value: String): String = if (value == null) "" else " = " + value
  private def renderFinalModifier(finalModifier: Boolean): String = if (finalModifier) "final " else ""
  private def renderFieldType(t: Type): String = t match {
    case p: Primitive => render(p)
    case Void => "Void"
    case TypeRef(pkg, name, generics) => name + renderGenerics(generics)
    case Class(_, _, name, _, _, _, _, generics) => name + renderGenerics(generics)
  }
  private def renderGenerics(generics: Seq[Generic]): String =
    if (generics.isEmpty) ""
    else "<" + generics.map(renderGeneric(_)).mkString(", ") + ">"
  private def renderGeneric(generic: Generic): String = generic match {
    case GenericType(t) => renderFieldType(t)
    case GenericTypeDeclaration(name, refType) => name
  }
  private def renderMethodBody(body: String): String = s"{\n$body\n}"
  private def renderName(name: String): String = " " + name
  private def renderPackage(pkg: Package): String = pkg.parts.mkString(".")
  private def renderPackageDeclaration(pkg: Option[Package]): String = pkg match {
    case Some(pkg) => "package " + renderPackage(pkg) + ";"
    case _ => ""
  }
  private def renderSection[T <: Renderable](elements: Iterable[T], separator: String = "\n"): String = if (elements.isEmpty) "" else "\n\n" + renderWithSeparator(elements, separator)
  private def renderStaticModifier(staticModifier: Boolean): String = if (staticModifier) "static " else ""
  private def renderWithSeparator[T <: Renderable](elements: Iterable[T], separator: String): String = elements.map(i => render(i)).mkString(separator)

}

sealed trait Renderable

case class Annotation(
  val pkg: Package,
  val name: String)
  extends Renderable

/**
 * A data type that can be a primitive type that are built in to the Java language or a reference type.
 */
sealed trait Type extends Renderable

/**
 * A simple data type is a primitive type or void.
 */
sealed trait SimpleType extends Type

/**
 * A data type that can be a primitive type that are built in to the Java language or a reference type.
 */
sealed trait ReferenceType extends Type {
  def isGenericType: Boolean
  def name: String
  def pkg: Option[Package]
}

/**
 * A definition of a reference type e.g. an interface, class or enum.
 */
sealed trait TypeDefinition extends ReferenceType with Renderable

case class CompilationUnit(val pkg: Option[Package], val imports: Set[TypeRef], val types: Seq[TypeDefinition]) extends Renderable

case class Package(val parts: Seq[String] = Seq()) {
  require(!parts.isEmpty, "package must not be empty")
}

case class TypeRef(
  val pkg: Option[Package],
  val name: String,
  val generics: Seq[Generic] = Seq())
  extends ReferenceType with Renderable {
  require(pkg.isDefined, "package must be defined")
  require(!name.isEmpty, "name must not be empty")
  def isGenericType = !generics.isEmpty
}

object TypeRef {
  def fullyQualifiedName(fqn: String, generics: Seq[Generic] = Seq()): TypeRef = {
    val parts = fqn.split('.')
    val pkg = Some(Package(parts.dropRight(1)))
    val name = parts.last
    TypeRef(pkg, name, generics)
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

sealed trait Generic
case class GenericType(val refType: ReferenceType) extends Generic
case class GenericTypeDeclaration(
  val name: String,
  val refType: ReferenceType)
  extends Generic {
  require(!name.trim.isEmpty, "name must not be blank")
}

case class Class(
  val accessModifier: AccessModifier,
  val finalModifier: Boolean,
  override val name: String,
  val fields: Seq[Field],
  val methods: Seq[Method],
  val types: Seq[TypeDefinition],
  val pkg: Option[Package] = None,
  val generics: Seq[Generic] = Seq())
  extends TypeDefinition {
  def isGenericType = !generics.isEmpty;
}

case class Field(
  val annotations: Seq[Annotation],
  val accessModifier: AccessModifier,
  val staticModifier: Boolean,
  val finalModifier: Boolean,
  val `type`: Type,
  val name: String,
  val value: String = null)
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
