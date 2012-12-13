package fst.systemf

import Syntax._;

/**
 * @author VUL IN AUB.
 */
class Evaluator(tcalc: TypeCalculator) {
    def tytermSubstTop(s:Type, t:Term): Term = null
    
    
    // shift de Bruijn indices (above cutoff c) by d (term-level)
    // corresponds to TAPL def. 6.2.1 p. 79 
    def shift(t:Term, d:Int, c:Int): Term = {
          t match {
                  case Zero => Zero
                  case Succ(t2) => Succ(shift(t2, d, c))
                  case Pred(t2) => Pred(shift(t2, d, c))
                  case IsZero(t2) => IsZero(shift(t2, d, c))
                  case True => True
                  case False => False
                  case If(t1, t2, t3) => If(shift(t1, d, c), shift(t2, d, c), shift(t3, d, c))                  
                  case App(t1, t2) =>  App(shift(t1, d, c), shift(t2, d, c))  
                  case TApp(t1, t2) => TApp(shift(t1, d, c), tcalc.tshift(t2, d, c))
                  case Var(i, n) => if (i<c) Var(i, n+d) else Var(i+d, n+d)
                  case Abs(nh, ty, t1) => Abs(nh, ty, shift(t1, d, c+1))
                  case TAbs(s, t) => TAbs(s, shift(t, d, c+1))
                  case Fixp() => Fixp() 
          }
    }
    
    
    // substitution with de Bruijn indices 
    // corresponds to TAPL def. 6.2.4 p. 80 
    def subst(t:Term, v:Int, s:Term) : Term = {  // [v<-s] t
          t match {
                  case Zero => Zero
                  case Succ(t2) => Succ(subst(t2,v,s))
                  case Pred(t2) => Pred(subst(t2,v,s))
                  case IsZero(t2) => IsZero(subst(t2,v,s))
                  case True => True
                  case False => False
                  case If(t1,t2,t3) => If(subst(t1,v,s),subst(t2,v,s),subst(t3,v,s))
                  case App(t1,t2) => App(subst(t1,v,s),subst(t2,v,s))
                  case TApp(t1, t2) => TApp(subst(t1, v, s), t2)
                  case Var(i,n) => if (i==v) s else Var(i,n)
                  case Abs(nh,ty,t1) => Abs(nh,ty,subst(t1,v+1,shift(s,1,0)))
                  case TAbs(nh, t) => TAbs(nh, subst(t, v+1, shift(s, 1, 0)))
                  case Fixp() => Fixp() 
          }
    }
    
    def tSubst(t:Term, v:Int, s:Type): Term = {
          t match {
                  case Zero => Zero
                  case Succ(t2) => Succ(tSubst(t2, v, s))
                  case Pred(t2) => Pred(tSubst(t2, v, s))
                  case IsZero(t2) => IsZero(tSubst(t2,v,s))
                  case True => True
                  case False => False
                  case If(t1,t2,t3) => If(tSubst(t1,v,s),tSubst(t2,v,s),tSubst(t3,v,s))
                  case App(t1,t2) => App(tSubst(t1,v,s),tSubst(t2,v,s))
                  case TApp(t1, t2) => TApp(tSubst(t1, v, s), t2)
                  case Var(i,n) => assert(i!=v); Var(i,n)
                  case Abs(nh,ty,t1) => {
                	  Abs(nh,tcalc.uniSubst(ty, v, s),tSubst(t1,v+1,tcalc.tshift(s,1,0)))
                  	}
                  case TAbs(nh, t) => TAbs(nh, tSubst(t, v+1, tcalc.tshift(s, 1, 0)))
                  case Fixp() => Fixp() 
          }
      
    }
    
    def termSubstTop(t:Term, s:Term): Term = {
          shift(subst(t,0,shift(s,1, 0)),-1,0)
    }
    
    def typeSubstTop(t:Term, s:Type): Term = {
          shift(tSubst(t,0,tcalc.tshift(s,1, 0)),-1,0)
    }
    
    case object NoRuleApplies extends Exception;
    
    // one step evaluation
    def eval1(t:Term): Term = {
            t match {
	            // Pierce p. 34
	            case If(True,t2,t3) => t2                                                 // E-IFTRUE
	            case If(False,t2,t3) => t3                                                // E-IFFALSE
	            case If(t1,t2,t3) => If(eval1(t1),t2,t3)                                    // E-IF
	            
	            // Pierce p. 41
	            case Succ(t1) => Succ(eval1(t1))                                          // E-SUCC
	            case Pred(Zero) => Zero                                                   // E-PREDZERO
	            case Pred(Succ(nv)) if nv.isNumVal => nv                                  // E-PREDSUCC
	            case Pred(t1) => Pred(eval1(t1))                                          // E-PRED
	            case IsZero(Zero) => True                                                 // E-ISZEROZERO
	            case IsZero(Succ(nv)) if nv.isNumVal => False                             // E-ISZEROSUCC
	            case IsZero(t1) => IsZero(eval1(t1))                                      // E-ISZERO
	
	            // Pierce p. 72
	            case App(Abs(nh,ty,t1),v2) if v2.isVal => termSubstTop(t1,v2)             // E-APPABS 
	            
	            
                case App(TApp(Fixp(), tT1), Abs(nh, ty, t2)) =>							  // E-FixBeta
                  		termSubstTop(t2, App(TApp(Fixp(), tT1), Abs(nh, ty, t2)))
	            
                case App(v1,t2) if v1.isVal => App(v1,eval1(t2))                          // E-APP2
	            case App(t1,t2) => App(eval1(t1),t2)                                      // E-APP1
	            
	            case TApp(TAbs(nh, t1), t2) => typeSubstTop(t1, t2)                       // E-TAPPTABS
	            case TApp(t1, t2) => TApp(eval1(t1), t2)								  // E-TAPP
	            
	            
	            case _ => throw NoRuleApplies
            }
            
    }

    // evaluation to normal form
    def eval(t:Term): Term = {
            try {
            	val tt = eval1(t)
            	
            	
                try{
                 val typer = new Typer(new TypeCalculator());
                 typer.typeOf(tt);
                }
            	catch{
            	  case _ => 
            	    println("Term before: " + t);
            	    println("Term exception : " + tt);
            	    throw new IllegalArgumentException;
            	}
            	
                eval(tt)
            }
            catch { 
                case NoRuleApplies => t 
            }
    }
}