import mep.Mep

object FibonacciTest extends Mep {
  def main(args:Array[String]) = {
    10 PRINT "Enter a number"
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
