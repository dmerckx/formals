package fst.common

case object NotSupportedException extends Exception {}

/**
 * A factory for syntax nodes used by the generic parsers 
 * DepParser and Parser. Inherit from this class, and override
 * all the features that your calculus supports.
 */
abstract class ICalculus[Term,Type,Kind,Constraint] {

    def notSupported = throw NotSupportedException;
    
    // essential Term constructs
    def mkVar(i : Int, n : Int) : Term;
    def mkApp(f : Term, a : Term) : Term
    def mkAbs(v: String, ty: Type, t : Term) : Term;

    // essential type constructs
    def mkTArr(t1: Type, t2: Type) : Type;

    // let bindings (optional)
    // default implementation via function application
    def mkLet(v : String, ty: Type, vi : Term, t : Term) : Term = mkApp(mkAbs(v,ty,t), vi);
    
    // parametric polymorphism (optional)
    def mkTAll(t : Type, v : String) : Type = notSupported;
    def mkTAbs(e : Term, v : String) : Term = notSupported;
    def mkTApp(e : Term, t : Type) : Term = notSupported;
    def mkTVar(i : Int, n : Int) : Type = notSupported;

    // Unit type (optional)
    def mkTUnit : Type = notSupported;
    def mkUnit : Term = notSupported;
    
    // Booleans (optional)
    def mkBool : Type = notSupported;
    def mkTrue : Term = notSupported;
    def mkFalse : Term = notSupported;
    def mkIfThenElse(c : Term, e1 : Term, e2 : Term) : Term = notSupported; 
    
    // integers (optional)
    def mkNat : Type = notSupported;
    def mkZero : Term = notSupported;
    def mkSucc(e : Term) : Term = notSupported;
    def mkPred(e : Term) : Term = notSupported;
    def mkIsZero(e : Term) : Term = notSupported;
    def mkNatLit(n : Int) : Term = if (n==0) mkZero else mkSucc(mkNatLit(n-1))
    
    // general recursion (optional)
    def mkFix(f : Term) : Term = notSupported;
    // fixp: fix as a primitive function which can be given a type (system f)
    def mkFixp : Term = notSupported
    
    // references (optional)
    def mkTRef(t : Type) : Type = notSupported;
    def mkRef(t : Term) : Term = notSupported;
    def mkDeref(r : Term) : Term = notSupported;
    def mkAssign(r: Term, v: Term) : Term = notSupported;
    
    // exceptions
    def mkTry(t1: Term, t2: Term) : Term = notSupported;
    def mkRaise(t: Term) : Term = notSupported

    // overloading (optional)
    def mkLetInst(e: Term, c: Constraint, ty: Type, o: Term) : Term = notSupported;
    def mkTAllC(t: Type, c: Constraint, v: String) : Type = notSupported;
    def mkTAbsC(e: Term, c: Constraint, v: String) : Term = notSupported;

    // overloading.. just equality primitive..
    def mkEq : Term = notSupported;
    def mkCEq : Constraint = notSupported;
    
    // overloading.. general..
    def mkCVar(i: Int, n: Int) : Constraint = notSupported;
    def mkLetClass(e : Term, mty : Type, cv : String, mv: String, tyv : String) : Term = notSupported;
    
    // dependent types...
    def mkPi(v: String, t1 : Type, t2: Type) : Type = notSupported;
    def mkSet : Type = notSupported;
    
    // bottom type
    def mkTBot : Type = notSupported;
    
    // singleton types
    def mkI : Term = notSupported;
    def mkRefl : Term = notSupported;
    def mkSubst : Term = notSupported;
    
    // structural recursion primitive for naturals
    def mkNatInd : Term = notSupported;
    
    // dependent if
    def mkBoolElim : Term = notSupported;
    
    // church bools, naturals, lists (context size passed for technical reason)
    // default implementation should be fine.
    def mkCBool(ctx: Int) : Type = notSupported;
    def mkCNat(ctx: Int) : Type = notSupported;
    def mkCList(x: Type, ctx: Int) : Type = notSupported; 
    def mkCMaybe(x: Type, ctx: Int) : Type = notSupported; 
    def mkCPair(x: Type, y: Type, ctx: Int) : Type = notSupported;
    
    // higher order polymorphism
    def mkStar : Kind = notSupported
    def mkKArr(k1: Kind, k2: Kind) : Kind = notSupported
    def mkTKAll(t : Type, k : Kind, v : String) : Type = notSupported;
    def mkTKAbs(e : Term, k : Kind, v : String) : Term = notSupported;
    def mkTyApp(t1 : Type, t2: Type) : Type = notSupported;
    def mkTyAbs(t : Type, k : Kind, v : String) : Type = notSupported;
}