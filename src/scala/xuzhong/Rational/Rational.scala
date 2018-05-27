

package scala.xuzhong.Rational

//Rational Class within its operations
class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1) //additional constructor

  println("Create " + n + "/" + d) //compile to constructor

  override def toString = n + "/" + d

  //add operation
  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  //+ operation
  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  //* operation
  def *(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  //< operation
  def lessThan(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  //max operation
  def max(that: Rational) =
    if (this.lessThan(that)) that else this

  //gcd operation
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

    //implicit transform: Int => Rational
  implicit def intToRational(x: Int): Rational = new Rational(x)
}