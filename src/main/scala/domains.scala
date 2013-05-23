
package monadic.domains

import monadic.syntax._
import TypeAliases._
// defined below

object TypeAliases {
	type Val = Clo
	type Addr = (Var, Time)
	type Time = Seq[CExp]
}

case class State(call: CExp, ρ: Env, σ: Store, t: Time)

case class Clo(lam: Lam, ρ: Env)

case class Env(m: Map[Var, Addr]) {
	def upd(entries: Seq[(Var, Addr)]) = {
		Env(m ++ entries.toMap)	
	}  

	def apply(v: Var) = m(v)
}

case class Store(m: Map[Addr, Set[Val]]) {
	def ⊔(entries: Seq[(Addr, Set[Val])]) = {
		Store(m ++ entries.toMap)
	} 

	def apply(a: Addr) = m(a)
}