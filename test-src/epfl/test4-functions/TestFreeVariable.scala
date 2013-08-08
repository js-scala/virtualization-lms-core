package scala.virtualization.lms.epfl.test4

import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.FileDiffSuite
import java.io.PrintWriter

class TestFreeVariable extends FileDiffSuite {

  val prefix = "test-out/epfl/test4-"

  trait FreeVariable extends Functions {

    // Random value
    def random(): Rep[Int]

    // Performs a side effect
    def effect(value: Rep[Int]): Rep[Unit]

    // Returns a function that performs a side-effect. Note that `value` is a free term.
    def makeFunction(value: Rep[Int]) = fun { (_: Rep[Unit]) => effect(value) }

  }

  trait FreeVariableExp extends FreeVariable with EffectExp {

    case class Noop[A]() extends Def[A]

    // Note the usage of reflectEffect
    def random() = reflectEffect(Noop[Int]())
    def effect(value: Exp[Int]) = reflectEffect(Noop[Unit]())

  }

  trait GenFreeVariable extends ScalaGenEffect {
    val IR: FreeVariableExp
    import IR._

    override def emitNode(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
      case Noop() =>
      case _ => super.emitNode(lhs, rhs)
    }
  }

  trait Prog extends Functions with FreeVariable {

    def test(ignored: Rep[Unit]): Rep[Unit] = {
      for (_ <- 1 to 50) {
        val value = random() // Get a random value
        val f = makeFunction(value) // Generate a function using this random value
        scala.Predef.println(s"$value, $f") // Print the returned symbols for the value and the function
      }
    }
  }

  def testFreeVar = {
    withOutFile(prefix + "free-var") {
      val prog = new Prog with TupledFunctionsRecursiveExp with FreeVariableExp
      val p = new ScalaGenFunctions with GenFreeVariable { val IR: prog.type = prog }
      p.emitSource(prog.test _, "test", new PrintWriter(System.out))
    }

  }

}
