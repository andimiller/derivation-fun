// example
case class Foo(a: String, number: Long) derives Show

@main def hello: Unit = {
  val f = Foo("hello", 123L)
  println(Show[Foo].show(f))
}

