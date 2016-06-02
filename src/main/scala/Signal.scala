import java.util.function.BiFunction
import java.util.function.Consumer
import java.util.function.Supplier
import Implicit._

class Signal[T](

  private var value: T

) extends Destructible with Supplier[T] {

  private var runnables: List[Runnable] = Nil

  def get() = value

  def set(t: T) {
    if (!destroyed) {
      value = t
      runnables.foreach(_ run)
    }
  }

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
          o.addParent(d.asInstanceOf[Destructible])
        }
      })
    })
    o
  }

  //Useful functions
  def buffer(f: Signal[Any]): Signal[List[T]] = {
    val s = new Signal(List[T]())
    var l = List(value)
    s addParent subscribe(() => l = l :+ value)
    s addParent f.subscribe(() => { s.set(l); l = Nil })
    s
  }

  def combineLatest[R, S](r: Signal[R], f: BiFunction[T, R, S]): Signal[S] = {
    val s = new Signal(f(value, r.value))
    s addParent subscribe(() => s.set(f(value, r.value)))
    s addParent r.subscribe(() => s.set(f(value, r.value)))
    s
  }

  def count(): Signal[Int] = {
    val s = new Signal(0)
    s addParent subscribe(() => s.set(s.value + 1))
    s
  }

  def distinct(): Signal[T] = {
    var old = value
    val s = filter(_ != old)
    s.subscribe(() => old = value)
    s
  }

  def filter(f: Function[T, Boolean]): Signal[T] = {
    val s = new Signal(value)
    s addParent subscribe(() => if (f(value)) s.set(value))
    s
  }

  def first(n: Int): Signal[T] = until(count().map(_ > n))

  def forEach(f: Consumer[T]) = subscribe(() => f.accept(value))

  def map[R](f: Function[T, R]): Signal[R] = {
    val s = new Signal(f(value))
    s addParent subscribe(() => s.set(f(value)))
    s
  }

  def ofType[R](c: Class[R]): Signal[R] = filter(c.isInstance(_)).map(c.cast)

  def reduce[R](r: R, f: BiFunction[T, R, R]): Signal[R] = {
    val s = new Signal(r)
    s addParent subscribe(() => s.set(f(value, s.value)))
    s
  }

  def unit(): Signal[Unit] = map(_ => ())

  def until(f: Supplier[Boolean]): Signal[T] = {
    val s = new Signal(value)
    s addParent subscribe(() => if (f.get) s.set(value)
    else s.destroy)
    s
  }
}

object Signal {

  def combine[T](a: Signal[T]*): Signal[T] = {
    val s = new Signal(a(0).value)
    a.foreach(s addParent _.forEach((v: T) => s.set(v)))
    s
  }
}
