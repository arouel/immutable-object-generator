package com.github.before.iog

object ImmutableObjectGenerator {

  def generate(model: Class, settings: Settings): String = {

    s"""package ${model.packageName}

public final class ${model.className} {
}"""

  }

}

case class Class(val packageName: String, val className: String)

case class Settings(val fieldPrefix: String = "")
