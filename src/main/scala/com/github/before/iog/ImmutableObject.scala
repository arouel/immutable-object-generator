package com.github.before.iog

object ImmutableObject {

  case class Member(
    val name: String,
    val `type`: Type,
    val accessorMethodPrefix: String = "get",
    val annotations: Seq[Annotation] = Seq())

  def accessorMethodName(member: Member): String = {
    val first = member.name.head.toUpper
    val rest = member.name.tail
    member.accessorMethodPrefix + first + rest
  }

  def accessorMethods(members: Seq[Member]): Seq[Method] = members.map(memberToAccessor(_))

  def define(pkg: Package, name: String, members: Seq[Member]): CompilationUnit = {
    val listType = TypeRef.fullyQualifiedName("java.util.List")
    val imports = typeRefs(members)
    val fields = this.fields(members)
    val methods = accessorMethods(members)
    val immutable = Class(Public, true, name, fields, Seq(), methods, Seq(), Some(pkg))
    val types = Seq(immutable)
    CompilationUnit(Some(pkg), imports, types)
  }

  def fields(members: Seq[Member]): Seq[Field] = members.map(memberToField(_))

  def memberToAccessor(m: Member): Method = Method(m.annotations, Public, false, false, m.`type`, Seq(), accessorMethodName(m), "return this." + m.name + ";")

  def memberToField(m: Member): Field = Field(m.annotations, Private, false, true, m.`type`, m.name, null)

  def typeRefs(fields: Iterable[Member]): Set[TypeRef] = {
    val types = fields.map(_.`type`)
    val empty: Set[TypeRef] = Set()
    types.foldLeft(empty)((set, t) => t match {
      case TypeRef(pkg, name, generics) =>
        if (pkg == Package(Seq("java", "lang"))) set
        else set + TypeRef(pkg, name, generics)
      case _ => set
    })
  }

}
