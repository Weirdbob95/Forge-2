import java.util.function.BiFunction
import java.util.function.Consumer
import java.util.function.Supplier
import java.util.function.Function

object Implicit {

  implicit def toRunnable(f: () => Unit) =
    new Runnable {
      def run = f()
    }

  implicit def toSupplier[T](f: () => T) =
    new Supplier[T] {
      def get = f()
    }

  implicit def toConsumer[T](f: T => Unit) =
    new Consumer[T] {
      def accept(t: T) = f(t)
    }

  implicit def toFunction[T, R](f: T => R) =
    new Function[T, R] {
      def apply(t: T) = f(t)
    }

  implicit def toBiFunction[T, R, S](f: (T, R) => S) =
    new BiFunction[T, R, S] {
      def apply(t: T, r: R) = f(t, r)
    }
}
