import fogus.baysick.Baysick

/**
 * Tests the ability to run everyone's first BASIC program; the endless loop
 * proclaiming self-virtue.
 *
 * Run as:
 *
 * <code>
 * java -cp $SCALA_HOME/lib/scala-library.jar:baysick.jar EndlessLoop
 * </code>
 *
 **/
object FloatBasicMathTest extends Baysick {
  def main(args:Array[String]) = {
    10 FLOAT ('a := 1.256f)
    20 FLOAT ('b := 4.6432f)
    30 FLOAT ('c := 'a + 'b)
    40 PRINT 'c
    50 END

    RUN
  }
}

object FloatMathTest extends Baysick {
  def main(args:Array[String]) = {
    10 FLOAT ('a := 1.253567f)
    20 FLOAT ('b := 3.5678f)
    30 PRINT 'a
    40 PRINT "Hello"
    50 END
    /*40 LET ('c := 'a + 'b)
    50 PRINT 'c
    60 LET ('c := 'b - 'a)
    70 PRINT 'c
    80 END*/

    RUN
  }
}
