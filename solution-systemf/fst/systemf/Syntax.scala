package fst.systemf

/**
 * @author VUL IN AUB.
 */
object Syntax {
    sealed abstract class Term { 
        def isVal : Boolean;
    }
    
    sealed abstract class Type {
      
    }
    
    // essentials
    case class Var (i : Int, n : Int) extends Term {
      override def isVal = true;
    }
    
    case class Abs(v : String, ty : Type, t : Term) extends Term {
      override def isVal = true;
      // alpha-equivalence...
      override def equals(o2 : Any) = o2 match {
        case Abs(_, ty_, t_) => t == t_ && ty == ty_;
        case _ => false
        };
    }
}