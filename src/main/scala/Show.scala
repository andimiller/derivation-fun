import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValueTuple, constValue}

trait Show[T]:
  def show(t: T): String
object Show:
  def apply[T: Show]: Show[T] = implicitly
  
  given Show[String] = new Show[String]:
    def show(s: String) = s
  given Show[Long] = new Show[Long]:
    def show(l: Long) = l.toString+"L"
  
  def static[T](s: String) = new Show[T] {
    override def show(t: T): String = s }
  
  // helpers to construct them
  inline def tryShow[T](x: T): String = summonInline[Show[T]].show(x)
  inline def showProduct[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Product): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val name = constValue[label]
            val value = tryShow(x.productElement(n).asInstanceOf[elem])
            s"$name = $value" :: showProduct[elems1, labels1](n+1)(x)
        }
      case _: EmptyTuple =>
        Nil
    }
    
  // okay derivation time
  inline given derived[T](using m: Mirror.Of[T]): Show[T] = {
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    new Show[T] {
      override def show(t: T): String = 
        m match {
        case p: Mirror.Product =>
          val label = constValue[m.MirroredLabel]
          inline m match {
            case m: Mirror.Singleton => label
            case _ =>
              label + "(" + showProduct[m.MirroredElemTypes, m.MirroredElemLabels](0)(t.asInstanceOf[Product]).mkString(", ") + ")"
          }
      }     
    }

  }

  inline def summonAll[T <: Tuple]: List[Show[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]

  type ProductAux[L, T] = Mirror.Of[T] { type MirroredElemLabels = L}
  
  inline def deriveProduct[L <: Tuple, T](m: ProductAux[L, T]): List[String] =
    constValueTuple[L].productIterator.asInstanceOf[Iterator[String]].toList

  inline def labels[L <: Tuple, T](using m: ProductAux[L, T]): List[String] =
    deriveProduct[L, T](m)
  