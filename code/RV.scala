package monad

import scala.util.Random

trait RV[+A] {
  override def toString(): String = {
    def s(t: RV[A], indent: String): String = t match {
      case l: Leaf[A] => indent + 
                         l.value.toString
      case n: Node[A] => s(n.right, indent + "    ") + 
                         "\n" + 
                         indent + 
                         n.value.toString + 
                         "\n" + 
                         s(n.left, indent + "    ")
    }
    s(this, "")
  }

  def getValue(word: List[Int]): A = this match {
    case n: Node[A] => if (word == List()) n.value
    else if (word.head == 0) n.left.getValue(word.tail)
    else n.right.getValue(word.tail)
    case l: Leaf[A] => l.value

  }

  def getTree(word: List[Int]): RV[A] = this match {
    case n: Node[A] => if (word == List()) n
    else if (word.head == 0) n.left.getTree(word.tail)
    else n.right.getTree(word.tail)
    case l: Leaf[A] => l
  }
  
  private def unit[B] (value: B) = Leaf(value)

  def map[B](f: A => B): RV[B] = this match {
    case l: Leaf[A] => Leaf(f(l.value))
    case n: Node[A] => Node(f(n.value), n.left map f, n.right map f)
  }

  def flatMap[B](f: A => RV[B]): RV[B] = {
    def kleisli(t: RV[A], word: List[Int]): RV[B] = {
      t match {
        case l: Leaf[A] => f(l.value).getTree(word)
        case n: Node[A] => Node(f(n.value).getValue(word), 
                                kleisli(n.left, word ++ List(0)), 
                                kleisli(n.right, word ++ List(1)))
      }
    }
    kleisli(this, List())
  }
  
  private def flatten[B] (tree: RV[RV[B]]): RV[B] = tree flatMap (x => x)
  
  private val oracle = new Random()
  
  def choose(): A = this match {
    case l: Leaf[A] => l.value
    case n: Node[A] => {
      if (oracle.nextBoolean) n.right.choose() else n.left.choose()
    }
  }
  
  def choose(num: Int): A = this match {
    case l: Leaf[A] => l.value
    case n: Node[A] => if (num==0) n.value
                       else if (oracle.nextBoolean) n.right.choose(num-1)
                       else n.left.choose(num-1)
  }
}

case class Node[A](value: A, left: RV[A], right: RV[A]) extends RV[A]

case class Leaf[A](value: A) extends RV[A]