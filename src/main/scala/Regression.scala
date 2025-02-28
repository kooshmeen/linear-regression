import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {
    val dataset = Dataset.apply(dataset_file)
    val (train, eval) = dataset.split(test_percentage)

    val X = Matrix(train.selectColumns(attribute_columns)) ++ 1.0
    val W = Matrix.initW(X.width)

    val yTrain = train.selectColumn(value_column)
    val Y = Matrix(yTrain)

    def gradientDescent(W: Matrix, X: Matrix, Y: Matrix, alpha: Double, steps: Int): Matrix = {
      @tailrec
      def gradientDescentRec(W: Matrix, X: Matrix, Y: Matrix, alpha: Double, steps: Int): Matrix = {
        if (steps == 0) W
        else {
          val predict = X * W
          val error = predict - Y
          val gradient = (X.transpose * error) ** (1.0 / Y.height.getOrElse(1))
          val newW = W - (gradient ** alpha)
          gradientDescentRec(newW, X, Y, alpha, steps - 1)
        }
      }

      gradientDescentRec(W, X, Y, alpha, steps)
    }

    val WFinal = gradientDescent(W, X, Y, alpha, gradient_descent_steps)

    def calculateMse(Y: Matrix, predict: Matrix): Double = {
      val error = predict - Y
      val squaredError = error.map(x => x * x)
      val sumSquaredError = squaredError.data.get.flatten.sum
      val mse = sumSquaredError / Y.height.getOrElse(1)
      mse
    }

    val mse = calculateMse(Y, X * WFinal)

    (WFinal, mse)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}