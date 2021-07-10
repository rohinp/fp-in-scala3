package features

object dot{}
/* val listModule = new { m =>
  type List = { this =>
    type Elem
    def head(): this.Elem
    def tail(): m.List & { type Elem <: this.Elem }
  }
  def nil() = new { this =>
      type Elem = Bot
      def head() = error()
      def tail() = error()
    }
  def cons[T](hd: T)(tl: m.List & { type Elem <: T }) = new { 
      this =>
      type Elem = T
      def head() = hd
      def tail() = tl
    }
} */