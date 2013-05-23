
package monadic.syntax

trait AExp

case class Var(name: String) extends AExp

case class Lam(params: Seq[Var], call: CExp) extends AExp

trait CExp

case class Call(f: AExp, args: Seq[AExp]) extends CExp

case object Exit extends CExp

