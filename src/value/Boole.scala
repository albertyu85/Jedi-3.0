package value

case class Boole(val value: Boolean) extends expression.Literal { 
  def &&(other: Boole) = new Boole(this.value && other.value)
  def || (other: Boole) = Boole(this.value || other.value)
  def not = Boole(!this.value)

  override def toString() = "" + value
}