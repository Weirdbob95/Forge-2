class MutableSignal[T](v: T) extends Signal[T](v) {

  def set(t: T) {
    if (!destroyed) {
      value = t
      runnables.foreach(_ run)
    }
  }
}
