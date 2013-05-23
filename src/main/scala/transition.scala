
package monadic.transition
import monadic.domains._
import monadic.domains.TypeAliases._
import monadic.syntax._

object next {
	def apply(ζ: State): Seq[State] = {
		ζ match {
			case State(Call(f, aes), ρ, σ, t) ⇒ {
				arg(f, ρ, σ) map {
					case proc@Clo(Lam(vs, call), ρ1) ⇒ {
						val t1 = tick(proc, ζ)
						val as = vs map { v ⇒ alloc(v, t1) }
						val ds = aes map { ae ⇒ arg(ae, ρ, σ) }
						val ρ2 = ρ1 upd (vs zip as)
						val σ1 = σ ⊔ (as zip ds)
						State(call, ρ2, σ1, t1) 
					}
				} toSeq
			}
			case ζ ⇒ Seq(ζ)
		}
	}
}

object arg {
	def apply(e: AExp, ρ: Env, σ: Store): Set[Val] = {
		e match {
			case v: Var ⇒ σ(ρ(v))
			case l: Lam ⇒ Set(Clo(l, ρ))
		}
	}
}

object tick {
	// this definition is for k-CFA
	val k = 2
	// the k in k-CFA
	def apply(c: Clo, ζ: State): Time = {
		ζ match {
			case State(call, _, _, t) ⇒ (call +: t) take k
		}
	}
}

object alloc {
	// for k-CFA
	def apply(v: Var, t: Time) = (v, t)
}
