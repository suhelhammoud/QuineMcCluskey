import scala.collection.mutable.ListBuffer
import scala.collection.{BitSet, mutable}

/**
  * Created by suhel on 12/22/15.
  */

case class Term(ones: BitSet, mask: BitSet) {
  private var isPrimTerm = true

  def setNonPrim {
    isPrimTerm = false
  }

  def nonEmpty = this != Term.NONE

  def isPrim = isPrimTerm

  def &(that: Term) = Term.compose(this, that)

  def onesSize = ones.size

  def maskSize = mask.size

  def toMiniTermsLines = {
    val basic = ones.map(1 << _).sum
    mask.subsets.map(s => s.map(1 << _).sum + basic).toList
  }

  //  override def toString = s"{ones: $ones, mask: $mask, isPrim: $isPrim}"

  def toBitString(length: Int = 0) = {
    val oLength = 1 + (if (ones.isEmpty) 0 else ones.max)
    val mLength = 1 + (if (mask.isEmpty) 0 else mask.max)
    val result = Array.fill(oLength max mLength max length)('0')
    ones.foreach(result(_) = '1')
    mask.foreach(result(_) = '_')
    result.mkString.reverse
  }

  def toNamedString(s: String) = {
    val names = s.reverse.split("\\s+")
    val oLength = 1 + (if (ones.isEmpty) 0 else ones.max)
    val mLength = 1 + (if (mask.isEmpty) 0 else mask.max)
    val result = Array.fill(oLength max mLength)("")
    (0 until result.length).map {
      case i if mask.contains(i) => ""
      case i if ones.contains(i) => names(i)
      case i if !(ones.contains(i)) => names(i) + "'"
    }.reverse.mkString
  }
}

object Term {

  def apply(n: Long): Term = new Term(toBitSet(n), BitSet())

  val NONE = Term(BitSet.empty, BitSet.empty)

  def compose(t1: Term, t2: Term): Term = {
    if (t1.mask != t2.mask) return NONE
    val msk = t1.ones ^ t2.ones
    if (msk.size != 1) return NONE
    Term(t1.ones - msk.head, msk ++ t2.mask)
  }

  def toBitSet(n: BigInt): BitSet =
    BitSet((0 until n.bitLength).filter(n.testBit(_)): _*)
}

object QuineMcCluskey {

  def composePrimMap(mp: mutable.Map[Int, ListBuffer[Term]]) = {
    val result = mutable.Map[Int, ListBuffer[Term]]()
    mp.keys.toList.sorted.sliding(2).foreach { p =>
      for {a <- mp.get(p(0)).get; b <- mp.get(p(1)).get} {

        val t = a & b
        if (t.nonEmpty) {
          a.setNonPrim
          b.setNonPrim
          if (result.get(t.onesSize).isEmpty) {
            result.put(t.onesSize, ListBuffer(t))
          } else {
            val lst = result.get(t.onesSize).get
            if (!(lst.contains(t)))
              lst += t
            //put list
          }
        }
      }

    }
    result
  }

  def getPrimTerms(mp: mutable.Map[Int, ListBuffer[Term]]): Array[Term] = {
    mp.values.map { e => e.filter(_.isPrim) }.flatten.toArray.distinct
  }

  def getAllPrimTerms(miniTerms: List[Term]) = {

    var mp = mutable.Map[Int, ListBuffer[Term]]()
    miniTerms.groupBy(_.onesSize).foreach(e => mp.put(e._1, ListBuffer(e._2: _*)))


    val result = ListBuffer[Term]()
    var continue = true
    while (continue) {
      val mpTemp = composePrimMap(mp)
      result ++= getPrimTerms(mp)
      if (mpTemp.size == 0) continue = false
      else {
        mp = mpTemp
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val lineNumbers = List(0, 1, 2, 5, 6, 7, 8, 9, 10, 14)
    val dLineNumbers = List() //TODO fix bugs
    val mTerms = lineNumbers.map(Term(_))
    val dTerms = dLineNumbers.map(Term(_))
    //    val lineNumbers = List(0, 1, 2, 5, 6, 7)
    val miniTerms = mTerms ++ dTerms

    val primTerms = getAllPrimTerms(miniTerms).filterNot( dTerms.contains)

    val coverMap = mutable.Map[Int, ListBuffer[Term]]()

    lineNumbers.foreach(i => coverMap += i -> ListBuffer[Term]())

    for {term <- primTerms
         line <- term.toMiniTermsLines
         if coverMap.contains(line)} { //TODO fix
      coverMap.get(line).get += term
    }

    val finalTerms = ListBuffer[Term]()
    while (coverMap.values.exists(_.size == 1)) {
      val primMap = coverMap.filter(e => e._2.size == 1)

      for (pEntry <- primMap if pEntry._2.nonEmpty) {
        coverMap -= pEntry._1
        val term = pEntry._2.head
        if (!(finalTerms.contains(term))) finalTerms += term
        for (line <- term.toMiniTermsLines) {
          val v = coverMap.get(line)
          if (v.nonEmpty) v.get -= term
        }
      }
      coverMap.filter(e => e._2.size == 0).foreach(coverMap -= _._1)
    }
    //    println(s"coverMap size : ${coverMap.size}")
    //    println(s"final Terms: \n${finalTerms.mkString("\n")}")

    println("In Bit representation")
    finalTerms.foreach(t => println(t.toBitString(4)))
    println("In Named Terms")
    finalTerms.foreach(t => println(t.toNamedString("A B C D")))

    //TODO for cyclic prime implicants use combinations to find the best solution
  }
}

