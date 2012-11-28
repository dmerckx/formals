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
  override def mkVar(i : Int, n : Int) = notSupported;
  override def mkAbs(v : String, ty: Type, t: Term) = notSupported;
  override def mkApp(f : Term, a : Term) = notSupported;
  
  override def mkTArr(t1 : Type, t2: Type) = notSupported;
  
  override def mkTAll(t: Type, v: String) = notSupported;
  override def mkTVar(i: Int, n: Int) = notSupported;
  override def mkTApp(t: Term, ty: Type) = notSupported;
  override def mkTAbs(t: Term, v: String) = notSupported;
  
  override def mkBool = notSupported;
  override def mkTrue = notSupported;
  override def mkFalse = notSupported;
  override def mkIfThenElse(c: Term, e1: Term, e2: Term) = notSupported;
  
  override def mkNat = notSupported;
  override def mkZero = notSupported;
  override def mkSucc(e: Term) = notSupported;
  override def mkPred(e: Term) = notSupported;
  override def mkIsZero(e: Term) = notSupported;
  
  override def mkFixp = notSupported;
  
  // TODO: implement
  // Church Booleans
  override def mkCBool(ctx: Int) : Type = mkTAll(mkTArr(mkTVar(0,ctx+1), mkTArr(mkTVar(0,ctx+1), mkTVar(0,ctx+1))), "X");
  def cfalseDef : String = """\X.\x:X.\y:X.y""";
  def ctrueDef : String = notSupported;
  def candDef : String = notSupported;
  def boolToCBool : String = notSupported;
  def cboolToBool : String = notSupported;
  
  // Church naturals
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCNat(ctx: Int) : Type = notSupported;
  def czeroDef : String = notSupported;
  def csuccDef : String = notSupported;
  def natToCNatDef : String = notSupported;
  def cnatToNatDef : String = notSupported;
  def cplusDef : String = notSupported;
  
  def ctimesDef : String = notSupported;
  def cexpDef : String = notSupported;

  // Church Maybe
  // ctx is the number of variables in the current ctx, 
  // which you may need to return a correct type here.
  override def mkCMaybe(x: Type, ctx: Int) : Type = notSupported;
  def cnothingDef : String = notSupported;
  def cjustDef : String = notSupported;
  def cmaybeDef : String = notSupported;
  def cmapDef : String = notSupported;

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