class Destructible {

  private var children: Set[Destructible] = Set()
  private var parents: Set[Destructible] = Set()

  protected var destroyed = false
  var name = "?"
  var id = { Destructible.maxID += 1; Destructible.maxID }

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

  def printHierarchy() {
    println(this)
    parents.foreach(d => {
      println("Has parent: " + d)
    })
    println()
    parents.foreach(d => {
      d.printHierarchy
    })
  }

  private def remChild(child: Destructible) {
    children = children diff Set(child)
    if (children.isEmpty)
      destroy
  }

  override def toString() = name + " (" + getClass.getSimpleName + " " + id + ")"
}

object Destructible {

  private var stack: List[Destructible] = Nil
  var maxID = 0

  def peek(): Option[Destructible] = if (stack.isEmpty) None else Some(stack(0))

  def pop(): Destructible = {
    stack match {
      case first :: rest =>
        stack = rest; first
      case List(el) =>
        stack = Nil; el
      case _ => null
    }
  }

  def push(d: Destructible) {
    stack = d :: stack
  }
}