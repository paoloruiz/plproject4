import mep.Mep

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
object FloatBasicMathTest extends Mep {
  def main(args:Array[String]) = {
    10 FLOAT ('a := 1.256f)
    20 FLOAT ('b := 4.6432f)
    30 LET ('c := 'a + 'a)
    40 PRINT 'b
    50 END

    RUN
  }
}

object IntBasicMathTest extends Mep {
  def main(args:Array[String]) = {
    10 INT ('a := 1)
    20 INT ('b := 4)
    30 INT ('c := 'a + 'a)
    40 PRINT 'b
    50 END

    RUN
  }
}

object FloatMathTest extends Mep {
  def main(args:Array[String]) = {
    10 FLOAT ('a := 1.253567f)
    20 FLOAT ('b := 3.5678f)
    30 PRINT 'a
    40 PRINT "Hello"
    50 END

    RUN
  }
}
