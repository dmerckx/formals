package fst.systemf

import fst.systemf.Syntax._;
import Syntax._

/**
 * @author VUL IN AUB.
 */
class TypeCalculator {
    def typeSubstTop(s:Type, t:Type): Type = {
    		tshift(uniSubst(t,0,tshift(s,1, 0)),-1,0)
    }
    
    def tshift(t:Type, d:Int, c:Int): Type = {
          t match {
                	case TBool => TBool
                	case TNat => TNat
                	case TArr(t1, t2) => TArr(tshift(t1, d, c), tshift(t2, d, c))
                	case TVar(i, n) => if (i<c) TVar(i, n+d) else TVar(i+d, n+d)
                	case TUni(s, t) => TUni(s, tshift(t, d, c+1))
          }
    }
    
    def uniSubst(t:Type, v:Int, s:Type) : Type = { // [V<-S] T
    	  t match {
                	case TBool => TBool
                	case TNat => TNat
                	case TArr(t1, t2) => TArr(uniSubst(t1, v, s), uniSubst(t2, v, s))
                	case TVar(i, n) => if(i==v) s else TVar(i, n)
                	case TUni(nh, t) => TUni(nh, uniSubst(t, v+1, tshift(s,1, 0)))
    	  }
    }
}