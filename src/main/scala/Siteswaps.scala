package siteswap

import java.io.FileWriter
import java.text.DecimalFormat

import scala.xml.NodeSeq


object Siteswaps {

  def isSiteswap(seq: List[Int]): Boolean = {
    val len = seq.size
    var pos: List[Int] = Nil
    //each object lands n beats later
    val targets = (seq zip (0 until len)).map(v => (v._1 + v._2) % len)
    //each object should land on a distinct beat
    targets.toSet == (0 until len).toSet
  }

  def normalizeSiteswap(seq: List[Int]): List[Int] = {
    var allSeq: List[List[Int]] = Nil
    var s = seq
    for (i <- 0 until seq.length) {
      allSeq ::= s
      s = s.tail :+ s.head
    }

    allSeq.maxBy(siteswapToInt)
  }

  //gen number for sorting
  def siteswapToInt(seq: List[Int]) = {
    var r = 0
    var m = 1
    for (v <- seq.reverse) {
      r += m * (v-1)
      m *= 10
    }
    r
  }

  def toLocal[T](seq: List[T]): List[T] = {
    var r: List[T] = Nil
    for (i <- (0 until seq.length).reverse)
      r ::= seq((i * 2) % seq.length)
    r
  }

  def isBoring(seq: List[Int]): Boolean = {
    //holds are boring
    if (seq contains 0) return true
    //no passing? seriously
    if (!(seq contains 5) && !(seq contains 7) && !(seq contains 9)) return true
    //flip-flip is boring
    if ((toLocal(seq)++toLocal(seq)) containsSlice List(4, 4)) return true
    false
  }

  def findSiteswaps(len: Int): Set[List[Int]] = {
    genSiteswaps(len).filterNot(isBoring).filter(isSiteswap).map(normalizeSiteswap).toSet
  }

  private def genSiteswaps(len: Int): List[List[Int]] = if (len == 0) List(Nil)
  else {
    for (sw <- genSiteswaps(len - 1);
         t <- Set(2, 4, 5, 6, 7, 8, 9, 10))
    yield t :: sw
  }

  def numberOfObjects(seq: List[Int]): Int = seq.sum / seq.length

  def siteswapToPrechac(seq: List[Int]): String =
    toLocal(seq).map(s => if (s % 2 == 0) (s / 2) else (s / 2) + ".5p").mkString(" ")

  def printSiteswap(seq: List[Int]): String =
    seq.map(s => if (s == 10) "a" else s.toString).mkString("")

  def getInterface(seq: List[Int]): String = {
    val len = seq.size
    var result = "s" * len
    var passes: Set[Int] = Set()

    for ((h, i) <- (seq zip (0 until len));
         if (h % 2 == 1))
      passes += (h + i) % len

    toLocal((for (i <- 0 until len)
    yield if (passes contains i) 'p' else 's').toList).mkString
  }

  def getNormalizedInterface(seq: List[Int]): String = {
    val interface = getInterface(seq)
    var allSeq: List[String] = Nil
    var s = interface
    for (i <- 0 until seq.length) {
      allSeq ::= s
      s = s.tail :+ s.head
    }

    allSeq.min
  }

  def toText(seq: List[Int]): String =
    toLocal(seq).map({
      case 0 => "hold"
      case 2 => "zip"
      case 4 => "flip"
      case 5 => "zap"
      case 6 => "self"
      case 7 => "pass"
      case 8 => "heff"
      case 9 => "double"
      case 10 => "triple"
    }).mkString(" ")

  def toPrechacThisLink(seq: List[Int]): String =
    "http://prechacthis.org/info.php?pattern=[" +
      toLocal(seq).map({
        case 0 => "0,0,0"
        case 2 => "1,0,1"
        case 4 => "2,0,2"
        case 5 => "2.5,1,"+((seq.length+1)/2+2)
        case 6 => "3,0,3"
        case 7 => "3.5,1,"+((seq.length+1)/2+3)
        case 8 => "4,0,4"
        case 9 => "4.5,1,"+((seq.length+1)/2+4)
        case 10 => "5,0,5"
      }).map("p(" + _ + ")").mkString(",") + "]&persons=2"


}

object NamedSiteswaps {

  import Siteswaps._

  val data =
    """
      |645 killer bunny
      |744 5 club one count
      |726 5 club one count
      |726 coconut laden swallow
      |942 glass elevator
      |867 French 3 count
      |966 7 club three count
      |756 baby dragon; zap opus 1; holy hand-grenade
      |945 dragon; black beast of aaaarg...
      |996 8 club pps
      |7 7 club one count
      |777 7 club one count
      |77777 7 club one count
      |7777777 7 club one count
      |9 9 club one count
      |999 9 club one count
      |99999 9 club one count
      |9999999 9 club one count
      |975 holy grail; zap opus two
      |64645 zap intro
      |86227 5 club why not
      |86722 5 club not why
      |86867 5 count popcorn (44)
      |77772 Martin's one count (async)
      |77722 parsnip
      |77222 inverted parsnip
      |86425 *
      |84562 *
      |88522 *
      |77466666 Jim's 3 count (async)
      |77466 Jim's 2 count (async)
      |774 Jim's 1 count (async)
      |7777266 mild madness (async)
      |77862 why not
      |77286 not why
      |78672 maybe
      |96672 not likely
      |79662 maybe not
      |77786 funky bookends
      |522 zap zip zip
      |552 zap zap zip
      |852 heff zip zap
      |825 heff zap zip
      |855 heff zap zap
      |885 heff heff zap
      |7a666 5 count popcorn
      |966a666 7 count popcorn (variation)
      |9668686 7 count popcorn (variation)
      |7a66686 7 count popcorn (variation)
      |786a666 7 count popcorn (variation)
      |7868686 7 count popcorn
      |9669964 7 club Jim's 2 count
      |9968926 7 club why not
      |9788926 7 club why not (variation)
      |9689962 7 club not why
      |9689782 7 club not why (variation)
      |7889962 7 club not why (variation)
      |7889782 7 club not why (variation)
      |9969268 7 club maybe (1)
      |9968296 7 club maybe (2)
      |9968278 7 club maybe (variation)
      |9669968926 why rei
      |9964786 7 club Jim's 2 count (variation)
      |9784966 7 club Jim's 2 count (variation)
      |9784786 7 club Jim's 2 count (variation)
      |b64 odd scots
      |767872782 self centered
      |7747746677466 Jim's ppsps (async)
      |8686777 Vitoria
    """.stripMargin

  def parse(line: String): (List[Int], String) = {
    val (sw, text) = line.splitAt(line.indexOf(" "))
    (normalizeSiteswap(sw.map(c => if (c == 'a') 10 else c.asDigit).toList), text.trim)
  }

  val namedSiteswaps = data.lines.filterNot(_.trim.isEmpty).map(parse).toMap

  def lookupName(seq: List[Int]) = namedSiteswaps.get(normalizeSiteswap(seq))
}

object SiteswapTests extends App {

  import Siteswaps._
  import NamedSiteswaps._

  def checkSiteswap(a: Int*): Unit = {
    assert(isSiteswap(a.toList), "%s is not a valid siteswap".format(a.mkString("")))
  }

  def invalidSiteswap(a: Int*): Unit = {
    assert(!isSiteswap(a.toList), "%s is a valid siteswap".format(a.mkString("")))
  }

  checkSiteswap(3)
  checkSiteswap(3, 4, 2)
  checkSiteswap(9, 7, 5)
  checkSiteswap(8, 6, 8, 6, 7)
  checkSiteswap(7, 2, 2, 2, 2)
  invalidSiteswap(5, 8)
  assert(isBoring(List(6,4,6,4,5)))
  assert(!isBoring(List(9,5,8,4,4)))


  def mkname(s: List[Int]) =
    lookupName(s). /*map(_+"; ").*/ getOrElse("") //+toText(s)

  var out = new FileWriter("csw.html")

  def run(len: Int) {
    val sw = findSiteswaps(len)
    for ((interface, sws) <- sw.groupBy(getNormalizedInterface).toList.sortBy(_._1.reverse).reverse) {
      println("\n" + interface)
      for (s <- sws.toList.sortBy(siteswapToInt).sortBy(numberOfObjects))
        println("%s\t\t%s\t\t%s\t\t%s".format(printSiteswap(s), siteswapToPrechac(s), new DecimalFormat("#.#").format(numberOfObjects(s).toFloat / 2.0), mkname(s)))
    }
  }

//  run(3)
//  run(5)
//  run(7)


  def genClassTags(s: List[Int]): String = {
    var tags:List[String] = List("sw")

    if (s contains 2) tags ::= "h2"
    if (s contains 4) tags ::= "h4"
    if (s contains 5) tags ::= "h5"
    if (s contains 6) tags ::= "h6"
    if (s contains 7) tags ::= "h7"
    if (s contains 8) tags ::= "h8"
    if (s contains 9) tags ::= "h9"
    if (s contains 10) tags ::= "ha"
    if (toLocal(s) containsSlice List(9,5)) tags ::= "dr"

    tags.mkString(" ")
  }

  def genHtml(len: Int): NodeSeq = {
    val sw = findSiteswaps(len)
    (for ((interface, sws) <- sw.groupBy(getNormalizedInterface).toList.sortBy(_._1.reverse).reverse) yield {
      val h = <h2>{interface}</h2>
      h ++ <p>{(for (s <- sws.toList.sortBy(siteswapToInt).sortBy(numberOfObjects)) yield
        <div class={genClassTags(s)}><span><a href={toPrechacThisLink(s)}>{printSiteswap(s)}</a></span> <span class="num">{new DecimalFormat("#.#").format(numberOfObjects(s).toFloat / 2.0)}</span> <span>{mkname(s)}</span></div>)}</p>
    }).flatten
  }

  scala.xml.XML.save("csw.html",
    <div>
      {genHtml(3) ++ genHtml(5) ++ genHtml(7)}
    </div>
  )



  //    .map(s=>println(printSiteswap(s)+ "   "+printSiteswap(toLocal(s))+"   "+numberOfObjects(s)+"   "+"   "+lookupName(s).getOrElse("")))


}

//object Main extends App {
//
//}
