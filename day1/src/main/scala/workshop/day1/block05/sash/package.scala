package workshop.day1.block05

import com.github.mvv.sash.{Effectful, Impurity, Purity}

import scala.language.experimental.macros

package object sash extends Effectful with Impurity with Purity {
  def effect[E, A](body: Preemptive[E, A]): Preemptive[E, A] = macro PreemptiveEffectMacro.effectImpl[E, A]
}
