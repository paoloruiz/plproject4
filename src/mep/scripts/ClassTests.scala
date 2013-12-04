import mep.Mep

object SortReverseClassTest extends Mep {
  def main(args:Array[String]) = {
    10 LIST ('a := List(3,1,2))
    20 PRINT 'a
    30 INPUT 'c
    40 SORT 'a
    50 PRINT 'a
    60 INPUT 'b
    70 REVERSE 'a
    80 PRINT 'a
    90 END

    RUN
  }
}

object LoopClassTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Loop Test"
    20 LIST ('a := List(5,1,4))
    30 PRINT 'a
    40 INPUT 'z
    50 PRINT "Printing elements of list"
    60 INPUT 'z
    70 FOR 'm IN 'a
    80 PRINT 'm
    90 ENDFOR

    100 PRINT "Printing range from 18 to 23"
    110 INPUT 'z
    120 FOR 'n IN RANGE(18,24)
    130 PRINT 'n
    140 ENDFOR

    150 END
   
    RUN
  }
}


object AccessClassTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Access and Set List elements test"
    20 INPUT 'z
    30 LIST ('a := RANGE(0,3))
    40 PRINT 'a
    50 PRINT "Setting element 0 to be 9"
    60 INPUT 'z
    70 SET('a, 0, 9)
    80 PRINT 'a
    90 PRINT "Getting 0th element"
    100 INPUT 'z
    110 PRINT GET('a,0)
    120 PRINT "Getting 1st element"
    130 INPUT 'z
    140 PRINT GET('a,1)
    150 END

    RUN
  }
}

object NestedWhileLoopTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Nested While loop test"
    20 INPUT 'z
    30 INT ('a := 0)
    40 INT ('b := 1)
    50 WHILE 'a <= 5
    60 PRINT "Printng 6 times"
    70 INT ('c := 0)
    80 WHILE 'c <= 1
    90 PRINT "Printing twice 6 times"
    100 INT ('c := 'c + 'b)
    110 ENDWHILE

    120 INT ('a := 'a + 'b)
    130 ENDWHILE

    140 END

    RUN
  }
}

object ExplicitTypeTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Explicit type test"
    20 INPUT 'z
    30 INT ('int := 42)
    40 PRINT 'int
    50 FLOAT ('float := 23.5)
    60 PRINT 'float
    70 LIST ('list := List(7,4,9))
    80 PRINT 'list
    90 STRING ('string := "String")
    100 PRINT 'string
    110 END

    RUN
  }
}

object FibonacciTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Fib: Enter a number"
    20 INPUT 'a
    30 INT ('b := 'a)
    40 INT ('f := 1)
    50 INT ('g := 0)
    60 FUNCTION 'm
    70 IF 'b > 1 THEN 120
    80 IF 'b === 1 THEN 100
    90 RETURN 'g
    100 INT ('h := 'h + 'f)
    110 RETURN 'f
    120 INT ('b := 'b - 'f)
    130 FUNCCALL 'm
    140 INT ('b := 'b - 'f)
    150 FUNCCALL 'm
    160 INT ('b := 'b + 'f + 'f)
    170 RETURN 'g
    180 ENDFUNC

    190 INT ('h := 0)
    200 FUNCCALL 'm
    210 PRINT 'h
    220 END

    RUN
  }
}
