package scala3_2

object homework1 {
  extension (x: String)
  def ++(y: String): Int = x.toInt + y.toInt

    @main def part1Ex(): Unit ={
      println("1" ++ "33")
    }
}

object homework2 {
  enum CompletionArg {
    case StringOpt(s: String)
    case IntOpt(i: Int)
    case FloatOpt(f: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = StringOpt(_)

    given fromInt: Conversion[Int, CompletionArg] = IntOpt(_)

    given fromFloat: Conversion[Float, CompletionArg] = FloatOpt(_)
  }
  import CompletionArg.*

  def complete(ca: CompletionArg) = ca match
    case StrType(s) => s"String type: $s"
    case IntType(i) => s"Int type: $i"
    case FloatType(f) => s"Float type: = $f"

  @main def part2Ex(): Unit ={
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm{
    def apply(d: Double) = math.log(d)
  }

  extension (x: Logarithm)
    def toDouble = math.exp(x)
    def +(y: Logarithm) = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm) = x + y


  @main def part3Ex(): Unit ={
    import Logarithm

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

  }
}