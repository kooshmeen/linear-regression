import Dataset._
import Regression._

import munit.FunSuite

class PlotTest extends FunSuite{
  val file = "datasets/houseds.csv"
  val split = 0.1
  val alpha = 1e-7
  val steps = 10000
  val (w, loss) = Regression.regression(file, List("YearBuilt", "GrLivArea"), "SalePrice", split, alpha, steps)
  println("y = " + w.transpose.data.get(0)(0) + "x1 + " +
    w.transpose.data.get(0)(1) + "x2 + " +
    w.transpose.data.get(0)(2))
  // y = 13.606240746925756x1 + 101.46591826674094x2 + -0.46562730905168315
}
