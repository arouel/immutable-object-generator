package com.github.before.iog

import org.specs2.mutable._
import ImmutableObjectGenerator._

class ImmutableObjectGeneratorSpec extends Specification {

  "The generator" should {
    
    "starts with package" in {
      generate(Class("com.github.before.iog.test", "MyTestClass"), Settings()) must startWith("package")
    }
 
    "ends with }" in {
      generate(Class("com.github.before.iog.test", "MyTestClass"), Settings()) must endWith("}")
    }
  
    "contains class name" in {
      generate(Class("com.github.before.iog.test", "MyTestClass"), Settings()) must contain("public final class MyTestClass ")
    }
  }
}