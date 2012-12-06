package fst.systemf

import fst.common.ICalculus
import fst.common.Parser
import Syntax._;

/**
 * This is the solution template for your implementation of System F.
 * 
 * @author VUL IN AUB.
 */
class Calculus extends ICalculus[Term,Type,Unit,Unit] {
  
  val parser = new Parser(this);
  
  type Term = Syntax.Term;
  type Type = Syntax.Type;
  
  // TODO: implement
  override def mkVar(i : Int, n : Int) = Var(i,n);
  override def mkAbs(v : String, ty: Type, t: Term) = Abs(v, ty, t);
  override def mkApp(f : Term, a : Term) = App(f, a); 
  
  override def mkTArr(t1 : Type, t2: Type) = TArr(t1, t2);
  
  override def mkTAll(t: Type, v: String) = TUni(v, t);
  override def mkTVar(i: Int, n: Int) = TVar(i, n);
  override def mkTApp(t: Term, ty: Type) = TApp(t, ty);
  override def mkTAbs(t: Term, v: String) = TAbs(v, t);
  
  override def mkBool = TBool;
  override def mkTrue = True;
  override def mkFalse = False;
  override def mkIfThenElse(c: Term, e1: Term, e2: Term) = If(c,e1,e2);
  
  override def mkNat = TNat;
  override def mkZero = Zero;
  override def mkSucc(e: Term) = Succ(e);
  override def mkPred(e: Term) = Pred(e);
  override def mkIsZero(e: Term) = IsZero(e);
  
  override def mkFixp = Fixp();
  
  // TODO: implement
  // Church Booleans
  override def mkCBool(ctx: Int) : Type = mkTAll(mkTArr(mkTVar(0,ctx+1), mkTArr(mkTVar(0,ctx+1), mkTVar(0,ctx+1))), "X");
  def cfalseDef : String = """\X.\x:X.\y:X.y""";

  def ctrueDef : String = """\X.\x:X.\y:X.x""";
  def candDef : String = """\b:CBool. \c:CBool. b[CBool] c (\X.\x:X.\y:X.y)""";
  def boolToCBool : String = """\b:Bool. if b then (\X.\x:X.\y:X.x) else (\X.\x:X.\y:X.y)""";
  def cboolToBool : String = """\b:CBool. b[Bool] true false""";
  
  // Church naturals
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCNat(ctx: Int) : Type = mkTAll( mkTArr( mkTArr(mkTVar(0, ctx+1), mkTVar(0, ctx+1)), mkTArr(mkTVar(0, ctx+1), mkTVar(0, ctx+1))), "X");
  def czeroDef : String = """\X. \s:X->X. \z:X. z""";
  def csuccDef : String = """\n:CNat. \X. \s:X->X. \z:X. s (n[X] s z)""";
  def natToCNatDef : String = """fixp[Nat->CNat]( \f:Nat->CNat. \n:Nat. \X. \s:X->X. \z:X. (if (iszero n) then z else s ((f (pred n))[X] s z  )    )  )""";
  def cnatToNatDef : String = """\n:CNat. n[Nat] (\m:Nat. succ m) 0""";
  def cplusDef : String = """\n:CNat. \m:CNat. \X. \s:X->X. \z:X. n[X] s (m[X] s z)""";
  
  def ctimesDef : String = """\n:CNat. \m:CNat. \X. \s:X->X. \z:X. n[X] (m[X] s) z""";
  def cexpDef : String = """\n:CNat. \m:CNat. \X. m[ X->X ] (n[X])""";

  // Church Maybe
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCMaybe(x: Type, ctx: Int) : Type = mkTAll( mkTArr(mkTVar(0,ctx+1), mkTArr(mkTArr(x, mkTVar(0, ctx+1)), mkTVar(0, ctx+1)  )  ), "X");
  def cnothingDef : String = """\A. \X. \x:X. \y:A->X. x""";
  def cjustDef : String = """\A. \a:A. \X. \x:X. \y:A->X. y a""";
  def cmaybeDef : String = """\X. \a:X. \b:CMaybe X. b[X] a (\x:X. x)""";
  def cmapDef : String = """\X. \Y. \f:X->Y. \c:CMaybe X.   \Z. \a:Z. \b:Y->Z.  c[Z] a (\x:X. b (f x))""";

  // Church pred
  def cpredDef : String = notSupported;

  // Church lists
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCList(x: Type, ctx: Int) : Type = notSupported;
  def cisnilDef : String = notSupported;
  def cnilDef : String = notSupported;
  def cconsDef : String = notSupported;

  // Church pairs
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCPair(x: Type, y: Type, ctx: Int) : Type = notSupported;
  def ccommaDef : String = notSupported;
  def cfstDef : String = notSupported;
  def csndDef : String = notSupported;

  // fibonacci
  def cfibDef : String = notSupported;

  // insertion sort
  def cinsertDef : String = notSupported;
  def cisortDef : String = notSupported;
}