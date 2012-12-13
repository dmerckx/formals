package fst.systemf

/**
 * @author VUL IN AUB.
 */
object Syntax {
    sealed abstract class Term { 
        def isVal = isNumVal;
        def isNumVal = false;
    }
    
    sealed abstract class Type {
    }
    
    // essentials
    
    //terms
    case class Var (i : Int, n : Int) extends Term {
      // note: i is the de Bruijn index, n is the total number of 
      // variables in scope for this term.  The second number is
      // not actually needed, but we keep track of it for consistency
      // checking and debugging.
      override def isVal = true;
      override def equals(v2: Any) = v2 match {
        case Var(i2, _) => i == i2
        case _ => false
      }
      override def toString = "(" + i + "," + n + ")"
    }
    case class Abs(v : String, ty : Type, t : Term) extends Term {
      override def isVal = true;
      // alpha-equivalence...
      override def equals(o2 : Any) = o2 match {
        case Abs(_, ty_, t_) => t == t_ && ty == ty_;
        case _ => false
        };
      override def toString = """\""" + v + ":" + ty + ". " + t;
    }
    case class App(f : Term, a : Term) extends Term {
      override def toString = "(" + f + ") " + a
    };
    
    case class TAbs(v : String, t : Term) extends Term {
      override def isVal = true;
      // alpha-equivalence...
      override def equals(o2 : Any) = o2 match {
        case TAbs(_, t) => t == t;
        case _ => false
        };
      override def toString = """\""" + v + ". " + t
    }
    
    case class TApp(f : Term, a : Type) extends Term{
      override def isVal = f match {
        case Fixp() => true
        case _ => false
      };
      override def toString = "("+ f + ")" + "[" + a + "]"
    }
    
    
    //types
    case class TVar(i: Int, n: Int) extends Type{
      override def equals(v2: Any) = v2 match {
        case TVar(i2, _) => i == i2
        case _ => false
      }
      override def toString = "(" + i + "," + n + ")"
    }
    
    
    
    case class TArr(t1 : Type, t2 : Type) extends Type {
      override def toString = "(" + t1 + " -> " + t2 + ")"
    };
    case class TUni(v : String, t : Type) extends Type{
      override def equals(o2 : Any) = o2 match {
        case TUni(_, t) => t == t;
        case _ => false
        };
      override def toString = "All " + v + ". " + t
    }
    
    
    
    
    
    
    // booleans
    case object TBool extends Type;
    case object True extends Term {
      override def isVal = true;
    }
    case object False extends Term {
      override def isVal = true;
    }
    case class If(c : Term, t1 : Term, t2: Term) extends Term {
      override def toString = "if " + c + " then " + t1 + " else " + t2
    };

    // naturals
    case object TNat extends Type;
    case object Zero extends Term {
      override def isNumVal = true;
    }
    case class Succ(e : Term) extends Term {
      override def isNumVal = e.isNumVal;
    }
    case class Pred(e : Term) extends Term;
    case class IsZero(e : Term) extends Term;
    
    
    // general recursion
    case class Fixp() extends Term{
      override def isVal = true;
    }
}