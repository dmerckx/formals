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
  val typer = new TypeCalculator();
  
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
  override def mkCMaybe(x: Type, ctx: Int) : Type = {
    mkTAll( mkTArr(mkTVar(0,ctx+1), mkTArr(mkTArr(typer.tshift(x, 1, 0), mkTVar(0, ctx+1)), mkTVar(0, ctx+1)  )  ), "Z");
  }
  def cnothingDef : String = """\A. \X. \x:X. \y:A->X. x""";
  def cjustDef : String = """\A. \a:A. \X. \x:X. \y:A->X. y a""";
  def cmaybeDef : String = """\X. \a:X. \b:CMaybe X. b[X] a (\x:X. x)""";
  def cmapDef : String = """\X. \Y. \f:X->Y. \c:CMaybe X.   \Z. \a:Z. \b:Y->Z.  c[Z] a (\x:X. b (f x))""";

  // Church pred
  def cpredDef : String = """\n:CNat. \X. \s:X->X. \z:X.
		  							let r : CMaybe X -> CMaybe X = \a:CMaybe X. \Z. \i:Z. \f:X->Z. f (a[X] z s) in  
		  							(n[CMaybe X] r (\Y. \x:Y. \y:X->Y. x) ) [X] z (\x:X. x)""";

  // Church lists
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCList(x: Type, ctx: Int) : Type ={
	mkTAll(mkTArr(mkTArr(typer.tshift(x, 1, 0), mkTArr(mkTVar(0, ctx+1), mkTVar(0, ctx+1))), mkTArr(mkTVar(0, ctx+1), mkTVar(0, ctx+1))), "R")
  }
  def cisnilDef : String = """\X. \l:CList X. l[Bool] (\hd:X. \tl:Bool. false) true""";
  def cnilDef : String = """\X. \R. \c:X->R->R. \n:R. n""";
  def cconsDef : String = """\X. \hd:X. \tl:CList X. (\R. \c:X->R->R. \n:R. c hd (tl[R] c n))""";

  // Church pairs
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCPair(x: Type, y: Type, ctx: Int) : Type ={
   mkTAll(mkTArr(mkTArr(typer.tshift(x, 1, 0),  mkTArr(typer.tshift(y, 1, 0), mkTVar(0, ctx+1))  ), mkTVar(0, ctx+1)), "R") 
  }
  def ccommaDef : String = """\X. \Y. \x:X. \y:Y. \R. \f:X->Y->R. f x y""";
  def cfstDef : String = """\X. \Y. \p:CPair X Y. p[X] (\x:X. \y:Y. x)""";
  def csndDef : String = """\X. \Y. \p:CPair X Y. p[Y] (\x:X. \y:Y. y)""";

  // fibonacci
  def cfibDef : String = """let f:CPair CNat CNat->CPair CNat CNat = \p:CPair CNat CNat. ccomma[CNat][CNat] (csnd[CNat][CNat] p) (cplus (cfst[CNat][CNat] p) (csnd[CNat][CNat] p)) in
		  					\n:CNat. cfst[CNat][CNat] ( n[CPair CNat CNat] f (ccomma[CNat][CNat] czero (csucc czero)) )""";

  // insertion sort
  def cinsertDef : String = """ \X. \lt:X->X->Bool. \l:CList X. \x:X. 
		  						let f : X -> CPair (CList X) Bool -> CPair (CList X) Bool
		  							= \h:X. \p:CPair (CList X) Bool. 
		  								let t:CList X = cfst[CList X][Bool] p in
		  									if (csnd[CList X][Bool] p)
		  									then ccons[X] h t
		  									else 
		  										if (lt x h)
		  										then ccomma[CList X][Bool] (ccons[X] h t) false
		  										else ccomma[CList X][Bool] (ccons[X] h (ccons[X] x t)) true
		  						in let r : CPair (CList X) Bool
		  							= l[CPair (CList X) Bool] f (ccomma[CList X][Bool] (cnil[X]) false)
		  						in if (csnd[CList X][Bool] r)
		  							then cfst[CList X][Bool] r
		  							else ccons[X] x (cfst[CList X][Bool] r)
		  					""";
  def cisortDef : String = """\X. \lt:X->X->Bool. \l:CList X. 
		  						let ins : X -> CList X -> CList X
		  							= \x:X. \t:CList X. cinsert[X] lt t x
		  						in l[CList X] ins (cnil[X])""";
}