import java.util.function.BiFunction
import java.util.function.Consumer
import java.util.function.Supplier
import Implicit._

class Signal[T](

  protected var value: T

) extends Destructible with Supplier[T] {

  protected var runnables: List[Runnable] = Nil

  private def createSignal[R](name: String, r: R, f: MutableSignal[R] => Any): MutableSignal[R] = {
    val s = new MutableSignal(r)
    s.name = name
    Destructible.push(s)
    f(s)
    Destructible.pop
    s
  }

  def get() = value

  def subscribe(run: Runnable): Observer = {
    runnables = runnables :+ run
    val o = new Observer(run)
    o.addParent(this)

    run.getClass.getDeclaredFields.foreach(f => {
      f.setAccessible(true)
      val func = f.get(run)
      func.getClass.getDeclaredFields.foreach(i => {
        i.setAccessible(true)
        val d = i.get(func)
        if (d.isInstanceOf[Destructible]) {
          if (Destructible.peek.isEmpty || Destructible.peek.get != d)
            o.addParent(d.asInstanceOf[Destructible])
        }
      })
    })
    if (Destructible.peek.isDefined) {
      Destructible.peek.get.addParent(o)
      o.name = "Observer[" + name + " -> " + Destructible.peek.get.name + "]"
    }
    o
  }

  //Useful functions
  def buffer(f: Signal[Any]): Signal[List[T]] = {
    createSignal("buffer", List[T](), s => {
      var l = List(value)
      subscribe(() => l = l :+ value)
      f.subscribe(() => { s.set(l); l = Nil })
    })
  }

  def combine(a: Signal[T]*): Signal[T] = {
    createSignal("combine", value, s => {
      forEach((v: T) => s.set(v))
      a.foreach(_.forEach((v: T) => s.set(v)))
    })
  }

  def combineLatest[R, S](r: Signal[R], f: BiFunction[T, R, S]): Signal[S] = {
    createSignal("combineLatest", f(value, r.value), s => {
      subscribe(() => s.set(f(value, r.value)))
      r.subscribe(() => s.set(f(value, r.value)))
    })
  }

  def count(): Signal[Int] = {
    createSignal("count", 0, s => subscribe(() => s.set(s.value + 1)))
  }

  def distinct(): Signal[T] = {
    var old = value
    val s = filter(_ != old)
    s.subscribe(() => old = value)
    s
  }

  def filter(f: Function[T, Boolean]): Signal[T] = {
    createSignal("filter", value, s => subscribe(() => if (f(value)) s.set(value)))
  }

  def first(n: Int): Signal[T] = until(count().map(_ > n))

  def forEach(f: Consumer[T]) = subscribe(() => f.accept(value))

  def map[R](f: Function[T, R]): Signal[R] = {
    createSignal("map", f(value), s => subscribe(() => s.set(f(value))))
  }

  def ofType[R](c: Class[R]): Signal[R] = filter(c.isInstance(_)).map(c.cast)

  def reduce[R](r: R, f: BiFunction[T, R, R]): Signal[R] = {
    createSignal("reduce", r, s => subscribe(() => s.set(f(value, s.value))))
  }

  def toUnit(): Signal[Unit] = map(_ => ())

  def until(f: Supplier[Boolean]): Signal[T] = {
    createSignal("until", value, s => subscribe(() => if (f.get) s.set(value) else s.destroy))
  }
}
