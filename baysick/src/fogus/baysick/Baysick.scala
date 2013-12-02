package fogus.baysick {
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.Stack

  /**
   * Implements a simplified, integer-only, dialect of the BASIC programming
   * language.  It implements a number of the BASIC forms and functions
   * including:
   *
   * <code>LET var := expression</code>
   * <code>PRINT expression [% expression]+</code>
   * <code>INPUT var</code>
   * <code>GOTO number</code>
   * <code>IF expression op expression THEN number</code>
   * <code>SQRT(expression)</code>
   * <code>ABS(expression)</code>
   *
   * It also provides simple math operators <code>* / - +</code> taking the
   * form:
   *
   * <code>expression op expression</code>
   *
   * Also provided are a few relation operators <code>< > <= >= ===</code>
   * taking the same structure as above.
   *
   * Variables are also expressions and are prepended by a single quote
   * (e.g. 'var).
   *
   * The functions below build a map of functions representing the program
   * expressions, variable access, and jumps keyed on the Int line number.
   *
   * @todo
   * 1.  Add GOSUB .. RETURN
   *     +  w/ subroutine map and return stack
   * 2.  Get a life
   *
   * @notes
   * Thanks to Szymon Jachim for the motivation to do this.
   *
   */
  class Baysick {
    abstract sealed class BasicLine
    case class PrintString(num: Int, s: String) extends BasicLine
    case class PrintResult(num:Int, fn:Function0[String]) extends BasicLine
    case class PrintFunctionResult(num:Int, fn:Function0[Int]) extends BasicLine
    case class PrintVariable(num: Int, s: Symbol) extends BasicLine
    case class PrintNumber(num: Int, number: BigInt) extends BasicLine
    case class Goto(num: Int, to: Int) extends BasicLine
    case class Input(num: Int, name: Symbol) extends BasicLine
    case class Let(num:Int, fn:Function0[Unit]) extends BasicLine
    case class LetInt(num:Int, fn:Function0[Unit]) extends BasicLine
    case class LetString(num:Int, fn:Function0[Unit]) extends BasicLine
    case class LetDouble(num:Int, fn:Function0[Unit]) extends BasicLine
    case class LetFloat(num:Int, fn:Function0[Unit]) extends BasicLine
    case class ListAssig(num:Int, fn:Function0[Unit]) extends BasicLine
    case class If(num:Int, fn:Function0[Boolean], thenJmp:Int) extends BasicLine
    case class While(num:Int, fn:Function0[Boolean]) extends BasicLine
    case class EndWhile(num: Int) extends BasicLine 
    case class ForRange(num: Int, v: Symbol, lis:List[Int]) extends BasicLine
    case class For(num: Int, v: Symbol, sym:Symbol) extends BasicLine
    case class EndFor(num: Int) extends BasicLine 
    case class End(num: Int) extends BasicLine
    case class Set(num:Int, sym:Symbol, ind:Int, value:Int) extends BasicLine
    /**
     * Bindings holds the two types of values provided, atoms and numerics.
     * It takes a type parameter on initialization corresponding to the
     * actual type.
     */
    class Bindings[T,U] {
      val atoms = HashMap[Symbol, T]()
      val numerics = HashMap[Symbol, U]()
      val lists = HashMap[Symbol, List[Int]]()

      /**
       * set uses a little hack to allow the storage of either one type or
       * another, but none other.
       */
      def set[X >: T with U](k:Symbol, v:X) = v match {
        case u:U => numerics(k) = u
        case t:T => atoms(k) = t
        case i:List[Int] => lists(k) = i
      }
      def atom(k:Symbol):T = atoms(k)
      def num(k:Symbol):U = numerics(k)
      def list(k:Symbol):List[Int] = lists(k)

      /**
       * Technically, you can have two variables with the same name with
       * different types at the same time, but for this version that does
       * not come into play.
       */
      def any(k:Symbol):Any = {
        (atoms.get(k), numerics.get(k), lists.get(k)) match {
          case (Some(x), None, None) => x
          case (None, Some(y), None) => y
          case (None, None, Some(z)) => z
          case (None, None, None) => None
          case (Some(x), Some(y), None) => Some(x,y)
          case (Some(x), None, Some(z)) => Some(x,z)
          case (None, Some(y), Some(z)) => Some(y,z)
          case (Some(x), Some(y), Some(z)) => Some(x,y,z)
        }
      }

      def contains(k:Symbol):Boolean = {
        return atoms.contains(k) || numerics.contains(k) || lists.contains(k)
      }
    }

    val lines = new HashMap[Int, BasicLine]
    val binds = new Bindings[String, Int]
    val floatBinds = new Bindings[String, Float]
    val returnStack = new Stack[Int]

    /**
     * The Assignment class is used by the `symbol2Assignment` implicit to
     * stand-in for a Scala symbol in the LET form.  This class returns
     * a function of () => Unit that does the appropriate binding.
     */
    case class Assignment(sym:Symbol) {
      def :=(v:String):Function0[Unit] = (() => binds.set(sym, v))
      def :=(v:Float):Function0[Unit] = (() => floatBinds.set(sym, v))
      def :=(v:Double):Function0[Unit] = (() => binds.set(sym, v))
      def :=(v:Int):Function0[Unit] = (() => binds.set(sym, v))
      def :=(v:List[Int]):Function0[Unit] = (() => binds.set(sym, v))
      def :=(v:Function0[Int]):Function0[Unit] = (() => binds.set(sym, v()))
      def :=[X: ClassManifest](v:Function0[List[Int]]):Function0[Unit] = (() => binds.set(sym, v()))
      def :=[X: ClassManifest, Y: ClassManifest](v:Function0[Float]):Function0[Unit] = (() => floatBinds.set(sym, v()))
    }

    /**
     * The MathFunction class is used by the `symbol2MathFunction` and
     * `fnOfInt2MathFunction` implicits to stand in for Scala symbols and
     * functions of type () => Int, the latter being constructed at run-time.
     */
    case class MathFunction(lhs:Function0[Int]) {
      def *(rhs:Int):Function0[Int] = (() => lhs() * rhs)
      def *(rhs:Function0[Int]):Function0[Int] = (() => lhs() * rhs())
      def /(rhs:Int):Function0[Int] = (() => lhs() / rhs)
      def /(rhs:Function0[Int]):Function0[Int] = (() => lhs() / rhs())
      def +(rhs:Symbol):Function0[Int] = (() => lhs() + binds.num(rhs))
      def +(rhs:Function0[Int]):Function0[Int] = (() => lhs() + rhs())
      def -(rhs:Symbol):Function0[Int] = (() => lhs() - binds.num(rhs))
      def -(rhs:Function0[Int]):Function0[Int] = (() => lhs() - rhs())

      // Math for Floats
/*      def *[X: ClassManifest](rhs:Float):Function0[Float] = (() => lhs() * rhs)
      def *[X: ClassManifest](rhs:Function0[Float]):Function0[Float] = (() => lhs() * rhs())
      def /[X: ClassManifest](rhs:Float):Function0[Float] = (() => lhs() / rhs)
      def /[X: ClassManifest](rhs:Function0[Float]):Function0[Float] = (() => lhs() / rhs())
      def +[X: ClassManifest](rhs:Symbol):Function0[Float] = (() => lhs() + floatBinds.num(rhs))
      def +[X: ClassManifest](rhs:Function0[Float]):Function0[Float] = (() => lhs() + rhs())
      def -[X: ClassManifest](rhs:Symbol):Function0[Float] = (() => lhs() - floatBinds.num(rhs))
      def -[X: ClassManifest](rhs:Function0[Float]):Function0[Float] = (() => lhs() - rhs())
*/
      // Math for Doubles
/*      def *[X: ClassManifest](rhs:Double):Function0[Double] = (() => lhs() * rhs)
      def *[X: ClassManifest](rhs:Function0[Double]):Function0[Double] = (() => lhs() * rhs())
      def /[X: ClassManifest](rhs:Double):Function0[Double] = (() => lhs() / rhs)
      def /[X: ClassManifest](rhs:Function0[Double]):Function0[Double] = (() => lhs() / rhs())
      def +[X: ClassManifest](rhs:Symbol):Function0[Double] = (() => lhs() + binds.num(rhs))
      def +[X: ClassManifest](rhs:Function0[Double]):Function0[Double] = (() => lhs() + rhs())
      def -[X: ClassManifest](rhs:Symbol):Function0[Double] = (() => lhs() - binds.num(rhs))
      def -[X: ClassManifest](rhs:Function0[Double]):Function0[Double] = (() => lhs() - rhs())
*/
    }


    case class MathFunctionFloat(lhs:Function0[Float]) {
      // Math for Floats
      def *(rhs:Float):Function0[Float] = (() => lhs() * rhs)
      def *(rhs:Function0[Float]):Function0[Float] = (() => lhs() * rhs())
      def /(rhs:Float):Function0[Float] = (() => lhs() / rhs)
      def /(rhs:Function0[Float]):Function0[Float] = (() => lhs() / rhs())
      def +(rhs:Symbol):Function0[Float] = (() => lhs() + floatBinds.num(rhs))
      def +(rhs:Function0[Float]):Function0[Float] = (() => lhs() + rhs())
      def -(rhs:Symbol):Function0[Float] = (() => lhs() - floatBinds.num(rhs))
      def -(rhs:Function0[Float]):Function0[Float] = (() => lhs() - rhs())
    }


    /**
     * The BinaryRelation class is used by the `symbol2BinaryRelation` and
     * `fnOfInt2BinaryRelation` implicits to stand in for Scala symbols and
     * functions of type () => Int, the latter being constructed at run-time.
     */
    case class BinaryRelation(lhs:Function0[Int]) {
      def ===(rhs:Int):Function0[Boolean] = (() => lhs()  == rhs)
      def <=(rhs:Int):Function0[Boolean] = (() => lhs() <= rhs)
      def <=(rhs:Symbol):Function0[Boolean] = (() => lhs() <= binds.num(rhs))
      def >=(rhs:Int):Function0[Boolean] = (() => lhs() >= rhs)
      def >=(rhs:Symbol):Function0[Boolean] = (() => lhs() >= binds.num(rhs))
      def <(rhs:Int):Function0[Boolean] = (() => lhs() < rhs)
      def >(rhs:Int):Function0[Boolean] = (() => lhs() > rhs)
    }

    /**
     * Branch provides the THEN part of an IF form which creates the If class
     * with the appropriate branching components.
     *
     * @arg num The line number of the IF form
     * @arg fn  The boolean function determining where the branch goes
     * @arg loc The THEN jump line number
     *
     */
    case class Branch(num:Int, fn:Function0[Boolean]) {
      def THEN(loc:Int) = lines(num) = If(num, fn, loc)
    }

    case class ForLoop(num:Int, v:Symbol) {
      def IN(lis:Symbol) = lines(num) = For(num, v, lis)
      def IN(lis:Function0[List[Int]]) = lines(num) = ForRange(num, v, lis())
    }

    case class WhileLoop(num:Int, fn:Function0[Boolean]) {
      lines(num) = While(num, fn)
    }

    def stringify(x:Any*):String = x.mkString("")

    /**
     * Appendr allows for the stringing together of expressions using the
     * `%` function.
     */
    case class Appendr(lhs:Any) {
      /**
       * <code>appendage</code> refers to the LHS value to be appended, <b>at
       * runtime</b>.  This is done, by setting it to a function which performs
       * lookups (for symbols) and toString conversion.
       *
       */
      var appendage = lhs match {
        case sym:Symbol => (() => binds.any(sym).toString)
        case fn:Function0[Any] => fn
        case _ => (() => lhs.toString)
      }

      def %(rhs:Any):Function0[String] = {
        /**
         * Check the type of the RHS.  For symbols, do a lookup, then
         * concatenate it to the result of the appendage function.
         */
        (() => rhs match {
          case sym:Symbol => stringify(appendage(), binds.any(sym))
          case fn:Function0[Any] => stringify(appendage(), fn())
          case _ => stringify(appendage(), rhs)
        })
      }
    }

    /**
     * Math Functions
     */
    def SQRT(i:Int):Function0[Int] = (() => Math.sqrt(i.intValue).intValue)
    def SQRT(s:Symbol):Function0[Int] = (() => Math.sqrt(binds.num(s)).intValue)
    def ABS(i:Int):Function0[Int] = (() => Math.abs(i))
    def ABS(s:Symbol):Function0[Int] = (() => Math.abs(binds.num(s)))

    /**
     * List Functions
     */
    def RANGE(s:Int,e:Int):Function0[List[Int]] = (() => List.range(s.intValue, e.intValue))
    def RANGE(s:Symbol,e:Int):Function0[List[Int]] = (() => List.range(binds.num(s).intValue, e.intValue))
    def RANGE(s:Int,e:Symbol):Function0[List[Int]] = (() => List.range(s.intValue, binds.num(e).intValue))
    def RANGE(s:Symbol,e:Symbol):Function0[List[Int]] = (() => List.range(binds.num(s).intValue, binds.num(e).intValue))
    
    def GET(s:Symbol, ind:Int):Function0[Int] = (() => castList(binds.num(s))(ind))

    def RUN() = gotoLine(lines.keys.toList.sortWith((l,r) => l < r).head)

    /**
     * LineBuilder is the jump off point for the line number syntax of
     * BASIC expressions.  Simply put, the `int2LineBuilder` implict
     * puts an instance of this class in place when it finds an Int
     * followed by one of the provided methods.  That is, this *only* handles
     * the forms that follow a line number and no other forms.  If you need to
     * add new functions that can be put elsewhere, then follow the form
     * used by SQRT and ABS.  For functions that can go *anywhere*, they should
     * be handled here and as separate functions as SQRT.
     */
    case class LineBuilder(num: Int) {
      def ENDWHILE() = lines(num) = EndWhile(num)
      def ENDFOR() = lines(num) = EndFor(num)
      def END() = lines(num) = End(num)
      object PRINT {
        def apply(str:String) = lines(num) = PrintString(num, str)
        def apply(number: BigInt) = lines(num) = PrintNumber(num, number)
        def apply(s: Symbol) = lines(num) = PrintVariable(num, s)
        def apply(fn:Function0[String]) = lines(num) = PrintResult(num, fn)
        def apply[X: ClassManifest](fn:Function0[Int]) = lines(num) = PrintFunctionResult(num, fn)
      }

      object INPUT {
        def apply(name: Symbol) = lines(num) = Input(num, name)
      }

      object SET {
        def apply(sym:Symbol, ind:Int, value:Int) = lines(num) = Set(num, sym, ind, value)
      }

      object LET {
        def apply(fn:Function0[Unit]) = lines(num) = Let(num, fn)
      }

      object INT {
        def apply(fn:Function0[Unit]) = lines(num) = LetInt(num, fn)
      }

      object STRING {
        def apply(fn:Function0[Unit]) = lines(num) = LetString(num, fn)
      }

      object DOUBLE {
        def apply(fn:Function0[Unit]) = lines(num) = LetDouble(num, fn)
      }

      object FLOAT {
        def apply(fn:Function0[Unit]) = lines(num) = LetFloat(num, fn)
      }

      object GOTO {
        def apply(to: Int) = lines(num) = Goto(num, to)
      }

      object IF {
        def apply(fn:Function0[Boolean]) = Branch(num, fn)
      }

      object WHILE {
        def apply(fn:Function0[Boolean]) = WhileLoop(num, fn)
      }

      object FOR {
        def apply(v:Symbol) = ForLoop(num, v) 
      }
     
      object LIST {
        def apply(fn:Function0[Unit]) = lines(num) = ListAssig(num, fn)
      }
    }
    
    private def castList(g:Any):List[Int] = {
      g match {
        case g2: List[Int] => return g2
        case _ => return List()
      }
    }

    private def whileLoopDone(line: Int, stackSize: Int) {
      lines(line) match {
        case While(_, fn:Function0[Boolean]) => {
          whileLoopDone(line + 10, stackSize + 1)
          return
        }
        case EndWhile(_) => {
          if (stackSize == 0) {
            gotoLine(line + 10)
            return
          } else {
            whileLoopDone(line + 10, stackSize -1)
          }
        }
        case _ => {
          whileLoopDone(line + 10, stackSize) 
        }
      }
    }
    
    private def forLoopDone(line: Int, stackSize: Int) {
      lines(line) match {
        case For(_, a:Any, b:Any) => {
          forLoopDone(line + 10, stackSize + 1)
          return
        }
        case EndFor(_) => {
          if (stackSize == 0) {
            gotoLine(line + 10)
            return
          } else {
            forLoopDone(line + 10, stackSize -1)
          }
        }
        case _ => {
          forLoopDone(line + 10, stackSize) 
        }
      }
    }
    
    private def whileLoopStart(line: Int, stackSize: Int) {
      lines(line) match {
        case EndWhile(_) => {
          whileLoopStart(line - 10, stackSize + 1)
          return
        }
        case While(_, fn:Function0[Boolean]) => {
          if (stackSize == 0) {
            gotoLine(line)
            return
          } else {
            whileLoopStart(line - 10, stackSize - 1)
          }
        }
        case _ => {
          whileLoopStart(line - 10, stackSize)
        }
      }
    }

    /**
     * This is the runtime evaluator of the built Scala classes from the
     * original BASIC forms.  Currently, lines can only be incremented by
     * 10, otherwise you program might not act the way you expect.
     */
    private def gotoLine(line: Int) {
      lines(line) match {
        case PrintNumber(_, number:BigInt) => {
          println(number)
          gotoLine(line + 10)
        }
        case PrintString(_, s:String) => {
          println(s)
          gotoLine(line + 10)
        }
        case PrintResult(_, fn:Function0[String]) => {
          println(fn())
          gotoLine(line + 10)
        }
        case PrintFunctionResult(_, fn:Function0[Int]) => {
          println(fn())
          gotoLine(line + 10)
        }
        case PrintVariable(_, s:Symbol) => {
          println(binds.any(s))
          gotoLine(line + 10)
        }
        case Input(_, name) => {
          var entry = readLine

          // Temporary hack
          try {
            binds.set(name, java.lang.Integer.parseInt(entry))
          }
          catch {
            case _ => binds.set(name, entry)
          }

          gotoLine(line + 10)
        }
        case Set(_, sym:Symbol, ind:Int, value:Int) => {
          binds.set(sym, castList(binds.num(sym)).updated(ind, value))
          gotoLine(line + 10)
        }
        case Let(_, fn:Function0[Unit]) => {
          fn()
          gotoLine(line + 10)
        }
        case LetInt(_, fn:Function0[Int]) => {
          fn()
          gotoLine(line + 10)
        }
        case LetString(_, fn:Function0[String]) => {
          fn()
          gotoLine(line + 10)
        }
        case LetDouble(_, fn:Function0[String]) => {
          fn()
          gotoLine(line + 10)
        }
        case LetFloat(_, fn:Function0[Float]) => {
          fn()
          gotoLine(line + 10)
        }
        case ListAssig(_, fn:Function0[Unit]) => {
          fn()
          gotoLine(line + 10)
        }
        case If(_, fn:Function0[Boolean], thenJmp:Int) => {
          if(fn()) {
            gotoLine(thenJmp)
          }
          else {
            gotoLine(line + 10)
          }
        }
        case While(_, fn:Function0[Boolean]) => {
          if(fn()) {
            gotoLine(line + 10)
          }
          else {
            whileLoopDone(line + 10, 0)
          }
        }
        case EndWhile(_) => {
          whileLoopStart(line - 10, 0)
        }
        case ForRange(_, v:Symbol, forlist:List[Int]) => {
          if (0 >= forlist.length) {
            forLoopDone(line+10, 0)
          }
          else {
            for (i <- 0 to forlist.length-1) {
              binds.set(v, forlist(i))
              gotoLine(line + 10)
            }
            forLoopDone(line+10, 0)
          }
        }
        case For(_, v:Symbol, sym:Symbol) => {
          var forlist:List[Int] = castList(binds.any(sym))
          if (0 >= forlist.length) {
            forLoopDone(line+10, 0)
          }
          else {
            for (i <- 0 to forlist.length-1) {
              binds.set(v, forlist(i))
              gotoLine(line + 10)
            }
            forLoopDone(line+10, 0)
          }
        }
        case EndFor(_) => {
          return
        }
        case Goto(_, to) => gotoLine(to)
        case End(_) => {
          println()
          println("BREAK IN LINE " + line)
        }
      }
    }

    implicit def int2LineBuilder(i: Int) = LineBuilder(i)
    implicit def toAppendr(key:Any) = Appendr(key)
    implicit def symbol2Assignment(sym:Symbol) = Assignment(sym)
    implicit def symbol2BinaryRelation(sym:Symbol) = BinaryRelation(() => binds.num(sym))
    implicit def fnOfInt2BinaryRelation(fn:Function0[Int]) = BinaryRelation(fn)
    implicit def symbol2MathFunction(sym:Symbol) = MathFunction(() => binds.num(sym))
    //implicit def symbol2MathFloatFunction(sym:Symbol) = MathFunctionFloat(() => floatBinds.num(sym))
    implicit def fnOfInt2MathFunction(fn:Function0[Int]) = MathFunction(fn)
  }
}
