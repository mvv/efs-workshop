package workshop.day1.block05.sash

import com.github.mvv.sash.EffectMacro
import workshop.day1.block05.Preemptive

import scala.reflect.macros.blackbox

object PreemptiveEffectMacro {
  def effectImpl[E, A](c: blackbox.Context)(
      body: c.Expr[Preemptive[E, A]]
  )(implicit errorTag: c.WeakTypeTag[E], resultTag: c.WeakTypeTag[A]): c.Expr[Preemptive[E, A]] = {
    import c.universe._
    val prefix = q"_root_.workshop.day1.block05.Preemptive"
    val unit = q"$prefix.unit"
    def flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { error: Tree =>
      q"$prefix.fail($error)"
    }
    val recover = { (action: Tree, handler: Tree) =>
      q"$action.catchSome($handler)"
    }
    val ensuring = { (action: Tree, cleanup: Tree) =>
      q"$action.ensuring($cleanup)"
    }
    val ensuringType = c.typecheck(unit).tpe
    val bodyType =
      c.typecheck(q"(null: _root_.workshop.day1.block05.Preemptive[${errorTag.tpe}, ${resultTag.tpe}])").tpe
    EffectMacro.effectImpl(c)(
      predef = Seq.empty,
      unit = Some(unit),
      flatMap = flatMap,
      raise = Some(raise),
      recover = Some(recover),
      ensuring = Some(ensuring),
      ensuringType = Some(ensuringType),
      body = body,
      bodyType = bodyType
    )
  }
}
