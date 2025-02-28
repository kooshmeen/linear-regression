import scala.annotation.tailrec
import scala.io.Source.fromFile

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  override def toString: String = {
    data.map(_.mkString(",")).mkString("\n")
  }

  def selectColumn(col: String): Dataset = {
    val idx = data.head.indexOf(col)
    new Dataset(data.map(row => List(row(idx))))
  }

  def selectColumns(cols: List[String]): Dataset = {
    val idxs = cols.map(col => data.head.indexOf(col))
    new Dataset(data.map(row => idxs.map(idx => row(idx))))
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sorted = data.tail.sortBy(_.head)
    val evalEntries = (1.0 / percentage - 1.0).ceil.toInt

    @tailrec
    def splitRec(entries: List[List[String]], eval: List[List[String]], train: List[List[String]],  acc: Int): (Dataset, Dataset) = {
      entries match {
        case Nil => (new Dataset(data.head :: train.reverse), new Dataset(data.head :: eval.reverse))
        case x :: xs if acc < evalEntries => splitRec(xs, eval, x :: train, acc + 1)
        case x :: xs => splitRec(xs, x :: eval, train, 0)
      }
    }
    splitRec(sorted, Nil, Nil, 0)
  }

  def size: Int = data.length
  def getRows: List[List[String]] = data
  def getHeader: List[String] = data.head
}

object Dataset {

  def apply(csv_filename: String): Dataset = {
    val bufferedSource = fromFile(csv_filename)
    val data = bufferedSource.getLines.map(line => line.split(",").toList).toList
    bufferedSource.close
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
  }
}
