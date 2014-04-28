package com.github.before.iog

object ImmutableObjectGenerator {

  def generate(model: CompilationUnit, settings: Settings): String = {
    Renderer.render(model)
  }

}

case class Settings(val fieldPrefix: String = "")
