class Destructible {

  private var children: Set[Destructible] = Set()
  private var parents: Set[Destructible] = Set()

  protected var destroyed = false

  def destroy(): Unit = {
    if (destroyed) return ()
    destroyed = true
    children.foreach(_ destroy)
    parents.foreach(_ remChild this)
    children = Set()
    parents = Set()
  }

  def addParent(parent: Destructible) {
    parents = parents + parent
    parent.children = parent.children + this
  }

  private def remChild(child: Destructible) {
    children = children diff Set(child)
    if (children.isEmpty)
      destroy
  }
}
