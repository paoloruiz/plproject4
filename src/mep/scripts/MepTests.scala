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
object FloatTest extends Mep {
  def main(args:Array[String]) = {
    10 FLOAT ('a := 1.253567)
    20 PRINT 'a
    30 END

    RUN
  }
}

object SortReverseTest extends Mep {
  def main(args:Array[String]) = {
    10 LIST ('a := List(5,3,9))
    20 PRINT 'a
    30 REVERSE 'a
    40 PRINT 'a
    50 SORT 'a
    60 PRINT 'a
    70 END

    RUN
  }
}

object FunctionTest extends Mep {
  def main(args:Array[String]) = {
    10 FUNCTION 'm
    20 INT ('a := 1)
    30 PRINT 'a
    40 ENDFUNC

    50 PRINT "This should print before"
    60 FUNCCALL 'm
    70 PRINT "Done with function"
    80 END

    RUN
  }
}

object FunctionReturnTest extends Mep {
  def main(args:Array[String]) = {
    10 FUNCTION 'm
    20 INT ('a := 1)
    30 RETURN 'a
    40 ENDFUNC

    50 PRINT "This should print before"
    60 FUNCCALL 'm
    70 POPRET 'b
    80 PRINT 'b
    90 PRINT "Should be a 1 up there"
    100 END

    RUN
  }
}

object AccessTest extends Mep {
  def main(args:Array[String]) = {
    10 LIST ('a := RANGE(0,3))
    20 PRINT 'a
    30 SET('a, 0, 9)
    40 PRINT 'a
    50 PRINT GET('a,0)
    60 PRINT GET('a,1)
    70 END

    RUN
  }
}

object EndlessLoop extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Fogus Rulez!"
    20 GOTO 10
    30 END

    RUN
  }
}

object RangeTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Range Test"
    20 LIST ('a := RANGE(0, 3))
    30 PRINT 'a
    40 END
    
    RUN
  }
}

object LoopTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Loop Test"
    20 LIST ('a := RANGE(0, 3))
    30 PRINT 'a
    40 FOR 'm IN 'a
    50 PRINT 'm
    60 ENDFOR 

    70 FOR 'n IN RANGE(18,20)
    80 PRINT 'n
    90 ENDFOR 

    100 END
    
    RUN
  }
}

object ListTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "List test"
    20 LIST ('a := List(0, 1, 2))
    30 PRINT 'a
    40 PRINT "List test2"
    50 END

    RUN
  }
}

object HelloWorld extends Mep {
  def main(args:Array[String]) = {
    10 INT ('a := 0)
    20 INT ('b := 1)
    30 WHILE 'a <= 5 
    40 PRINT "Hello Cleveland!"
    50 INT ('c := 0)
    60 WHILE 'c <= 2
    70 PRINT "Hello Austin"
    80 INT ('c := 'c + 'b)
    90 ENDWHILE 

    100 INT ('a := 'a + 'b)
    110 ENDWHILE
 
    120 END

    RUN
  }
}

object HelloNumbers extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Here are some numbers:"
    20 PRINT 42
    30 PRINT 102845646545646546L
    40 PRINT "... and some math"
    50 PRINT "2 + 2"
    60 PRINT 2 + 2
    70 PRINT "102845646545646546 * 2"
    80 PRINT 102845646545646546L * 2
    90 PRINT "5 - 100"
    100 PRINT 5 - 100
    110 PRINT "3/2"
    120 PRINT 3 / 2
    130 END

    RUN
  }
}

object HelloName extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "What is your name: "
    20 INPUT 'a
    30 PRINT "Hello " % 'a
    40 END

    RUN
  }
}

object HelloPrint extends Mep {
  def main(args:Array[String]) = {
    10 STRING ('a := "World")
    20 PRINT "Hello " % 'a
    30 PRINT 'a % " World"
    40 INT ('a := 42)
    50 PRINT "Hello " % 42
    60 PRINT "Hello " % 'a
    70 PRINT 42 % " World"
    80 PRINT 'a % " World"
    90 PRINT 'a % 'a
    100 PRINT 42 % 42  // doesn't work!  :(  calls modulo
    110 PRINT "Hello " % "World"
    120 END

    RUN
  }
}

object HelloLet extends Mep {
  def main(args:Array[String]) = {
    10 STRING ('a := "Hello Let!")  // weird, LET requires parens
    20 PRINT 'a
    30 INT ('a := 42)
    40 PRINT 'a
    50 END

    RUN
  }
}

object HelloIf extends Mep {
  def main(args:Array[String]) = {
    10 INT ('a := 5)
    20 IF 'a === 5 THEN 40
    30 PRINT "This will never execute"
    40 PRINT "They were equal!"
    50 IF 'a === 6 THEN 70
    60 PRINT "This will execute 1st..."
    70 PRINT "...then this!"
    80 END

    RUN
  }
}

object SquareRoot extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Enter a number"
    20 INPUT 'n
    30 PRINT "Square root of " % "'n is " % SQRT('n)
    40 END

    RUN
  }
}

object Lunar extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Welcome to Mep Lunar Lander v0.0.1"
    20 INT ('dist := 100)
    30 INT ('v := 1)
    40 INT ('fuel := 1000)
    50 INT ('mass := 1000)

    60 PRINT "You are a in control of a lunar lander."
    70 PRINT "You are drifting towards the surface of the moon."
    80 PRINT "Each turn you must decide how much fuel to burn."
    90 PRINT "To accelerate enter a positive number, to decelerate a negative"

    100 PRINT "Distance " % 'dist % "km, " % "Velocity " % 'v % "km/s, " % "Fuel " % 'fuel
    110 INPUT 'burn
    120 IF ABS('burn) <= 'fuel THEN 150
    130 PRINT "You don't have that much fuel"
    140 GOTO 100
    150 INT ('v := 'v + 'burn * 10 / ('fuel + 'mass))
    160 INT ('fuel := 'fuel - ABS('burn))
    170 INT ('dist := 'dist - 'v)
    180 IF 'dist > 0 THEN 100
    190 PRINT "You have hit the surface"
    200 IF 'v < 3 THEN 240
    210 PRINT "Hit surface too fast (" % 'v % ")km/s"
    220 PRINT "You Crashed!"
    230 GOTO 250
    240 PRINT "Well done"

    250 END

    RUN
  }
}
