import scala.annotation.targetName

type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    data match {
      case Some(value) => {
        val data = value.head.indices.map {
          i => value.indices.map {
            j => value(j)(i)
          }.toList
        }.toList
        new Matrix(Some(data))
      }
      case None => new Matrix(None)
    }
  }

  def map(f: Double => Double): Matrix = {
    data match {
      case Some(value) => {
        val data = value.map(row => row.map(f))
        new Matrix(Some(data))
      }
      case None => new Matrix(None)
    }
  }

  @targetName("multTwoMatrices")
  def *(other: Matrix): Matrix = {
    (data, other.data) match {
      case (Some(value1), Some(value2)) if value1.head.length == value2.length => {
        val data = value1.indices.map { i =>
          value2.head.indices.map { j =>
            value1(i).zip(value2.map(_(j))).map { case (a, b) => a * b }.sum
          }.toList
        }.toList
        new Matrix(Some(data))
      }
      case _ => new Matrix(None)
    }
  }

  @targetName("addCol")
  def ++(x: Double): Matrix = {
    data match {
      case Some(value) => {
        val data = value.map(row => row :+ x)
        new Matrix(Some(data))
      }
      case None => new Matrix(None)
    }
  }

  @targetName("subtractTwoMatrices")
  def -(other: Matrix): Matrix = {
    (data, other.data) match {
      case (Some(value1), Some(value2)) if value1.length == value2.length && value1.head.length == value2.head.length => {
        val data = value1.zip(value2).map {
          case (row1, row2) => row1.zip(row2).map {
            case (a, b) => a - b
          }
        }
        new Matrix(Some(data))
      }
      case (Some(value1), Some(value2)) => new Matrix(None)
      case _ => new Matrix(None)
    }
  }
  @targetName("multMatrixByScalar")
  def **(x: Double): Matrix = {
    data match {
      case Some(value) => {
        val data = value.map(row => row.map(_ * x))
        new Matrix(Some(data))
      }
      case None => new Matrix(None)
    }
  }

  def data: Option[Mat] = m
  def height: Option[Int] = m match {
    case Some(value) => Some(value.length)
    case None => None
  }
  def width: Option[Int] = m match {
    case Some(value) => Some(value.head.length)
    case None => None
  }

  override def toString: String = {
    data match {
      case Some(value) => value.map(_.mkString(",")).mkString("\n")
      case None => throw new Exception("tostring exception")
    }
  }
}

object Matrix {

  def apply(data: Mat): Matrix = {
    new Matrix(Some(data))
  }

  def apply(data: Option[Mat]): Matrix = {
    new Matrix(data)
  }

  def apply(dataset: Dataset): Matrix = {
    val datasetNoHeader = dataset.getRows.tail
    val data = datasetNoHeader.map(_.map(_.toDouble))
    new Matrix(Some(data))
  }

  def initW(columns: Option[Int]): Matrix = {
    columns match {
      case Some(value) => {
        val data = List.fill(value)(List(0.0))
        new Matrix(Some(data))
      }
      case None => new Matrix(None)
    }

  }
}
