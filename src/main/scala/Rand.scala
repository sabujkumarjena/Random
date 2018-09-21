package rand

trait Rand[T] {
  self =>
  def gen: T
  def map[U](f: T => U): Rand[U] =
    new Rand[U] {
      def gen = f(self.gen)
    }
  def flatMap[U](f: T => Rand[U]):Rand[U] = 
    new Rand[U]{
    def gen = f(self.gen).gen
  }
}

object Rand{
  import scala.util.Random
  def apply[T](implicit rg:Rand[T]) = rg
  
  implicit val randInt:Rand[Int] = new Rand[Int]{
    def gen:Int = new Random().nextInt()
  }
  implicit val randDouble:Rand[Double] = new Rand[Double]{
    def gen:Double = new Random().nextDouble()
  }
  implicit val randFloat:Rand[Float] = new Rand[Float]{
    def gen:Float = new Random().nextFloat()
  }
  implicit val randLong:Rand[Long] = new Rand[Long]{
    def gen:Long = new Random().nextLong()
  }
  implicit val randBoolean:Rand[Boolean] = new Rand[Boolean]{
    def gen:Boolean = new Random().nextBoolean()
  }
  implicit val randChar:Rand[Char] = new Rand[Char]{
    def gen:Char = new Random().nextPrintableChar()
  }
  def const[T](x:T):Rand[T] = new Rand[T]{
    def gen:T = x
  }
  
  def list: Rand[List[Int]] = for (b <- Rand[Boolean] )yield
    if(b) emptyList.gen
    else nonEmpty.gen
  def emptyList: Rand[List[Int]] = const[List[Int]](Nil)
  
  def nonEmpty : Rand[List[Int]] = for(i <- Rand[Int]) yield (i :: list.gen)
  
  
}

