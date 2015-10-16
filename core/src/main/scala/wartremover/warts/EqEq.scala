package org.brianmckenna.wartremover
package warts

import org.brianmckenna.wartremover.{WartTraverser, WartUniverse}

// TODO: rename EqEqExEq
// TODO: add wart to README
object EqEq extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    import scala.reflect.NameTransformer

    val EqEqName: TermName = NameTransformer.encode("==")
    val ExEqName: TermName = NameTransformer.encode("!=")

    new Traverser {
      override def traverse(tree: Tree) {
        val synthetic = isSynthetic(u)(tree)
        // TODO : remove debug
        // println(s"synthetic: $synthetic\ntree: $tree\nraw: ${showRaw(tree)}\n")
        tree match {
          // ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          // pattern matching outputs a synthetic ==
          case LabelDef(_, _, If(_, a, b)) if synthetic =>
            traverse(a)
            traverse(b)
          // case class outputs a synthetic ==
          case DefDef(_, _, _, _, _, _) if synthetic =>
          case rt: RefTree if rt.name == EqEqName => u.error(tree.pos, "== is disabled")
          case rt: RefTree if rt.name == ExEqName => u.error(tree.pos, "!= is disabled")
          case _ => super.traverse(tree)
        }
      }
    }
  }
}
