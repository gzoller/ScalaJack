package co.blocke.scalajack
package internal

case class TreeNode[P](payload: P, children: List[TreeNode[P]] = Nil):
  def addChild(tn: TreeNode[P]) = this.copy(children = this.children :+ tn)
  inline def hasChildren = !children.isEmpty

object TreeNode:

  def inverted[P](tn: TreeNode[P]) = deapthFirst(tn).reverse
  def deapthFirst[P](tn: TreeNode[P], acc: List[TreeNode[P]] = Nil): List[TreeNode[P]] =
    tn.children.foldLeft(if acc == Nil then List(tn) else acc) { case (soFar, child) =>
      val nextList = soFar :+ child
      if !child.hasChildren then nextList
      else deapthFirst(child, nextList)
    }
