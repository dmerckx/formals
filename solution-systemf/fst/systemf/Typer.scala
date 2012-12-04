package fst.systemf

import Syntax._

case object TypeException extends Exception;

/**
 * @author VUL IN AUB.
 */
class Typer(tcalc: TypeCalculator) {
  type Context = List[Either[Type, Unit]]
  def typeOf(t: Term,ctx: Context): Type = {
        println("context: " + ctx);
		println("term: " + t);
		t match {
                  case Zero => TNat                                          // T-ZERO
                  case Succ(t2) =>                                            // T-SUCC
                        if (typeOf(t2,ctx) == TNat) TNat 
                        else throw TypeException
                  case Pred(t2) =>                                            // T-PRED
                    if (typeOf(t2,ctx) == TNat) TNat 
                    else throw TypeException
                  case IsZero(t2) =>                                          // T-ISZERO
                    if (typeOf(t2,ctx) == TNat) TBool 
                    else throw TypeException
                  case True => TBool                                         // T-TRUE
                  case False => TBool                                        // T-FALSE
                  case If(t1,t2,t3) => {                                      // T-IF
                        if (typeOf(t1,ctx) == TBool) {
                           val T2= typeOf(t2,ctx)
                           val T3= typeOf(t3,ctx)
                           if (T2==T3) T2
                           else throw TypeException
                        } else throw TypeException
                  }
                  case App(t1,t2) => {                                          // T-APP
                        val tT1 = typeOf(t1,ctx)
                        val tT2 = typeOf(t2,ctx)
                        tT1 match {
                            case TArr(tT11,tT12) =>
                            if(tT11 == tT2){
                              tT12
                            }
                            else {
	                            println(" Type left: " + tT11);
	                            println(" Type right: " + tT12);
                              throw TypeException
                            }
                            case _ => throw TypeException
                        }
                  }
                  case TApp(t1, t2) => {										// T-TAPP
                	   val tT1 = typeOf(t1, ctx)
                	   tT1 match {
                	     	case TUni(v, t12) =>
                	     	  val t = tcalc.typeSubstTop(t2, t12)
                	     	  println("type of TUni: " + t);
                	     	  t
                            case _ => throw TypeException
                	   }
                  }
                  case Var(i,n) => {											// T-VAR 
                    ctx(i) match {
                      case Left(j) => 
                        println("   -type: " + j);
                        tcalc.tshift(j, i+1, 0)
                      case _ => throw TypeException
                    }
                  }
                  case Abs(nh,tT,t1) => {                                       // T-ABS
                        val tT1 = typeOf(t1,(Left(tT) :: ctx));
                        println("   type: " + tT1);
                        
                        TArr(tT,tcalc.tshift(tT1, -1, 0))
                  }
                  case TAbs(nh, t2) => {										// T-TABS
                        val tT2 = typeOf(t2,(Right() :: ctx));
                        println("    TAbs type: " + tT2);
                        
                        TUni(nh, tT2)
                  }
        }
  }
  def typeOf(t: Term) : Type = typeOf(t, Nil);
}