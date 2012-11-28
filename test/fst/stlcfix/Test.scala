package fst.stlcfix

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import fst.common.Parser
import Syntax._
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {
    val calc = new Calculus();
    val parser = new Parser(calc);
    val eval = new Evaluator;
    val typer = new Typer;

    var i = 0;
    def parseTest(in: String, out: Term) {
        test("parse expression " + i + ": '" + in + "'") {
            val e = parser.parseTerm(in);
            e should be ===(out);
        }
        i = i + 1
    }

    def evaluateAndTypeTest(in:String, ty: Type, out: Term) {
        test("type and evaluate expression " + i + ": '" + in + "'") {
            val e : Term = parser.parseTerm(in);
            if( ty != null) {
                typer.typeOf(e,Nil) should be ===(ty);
                if(out != null) { eval.eval(e) should be ===(out) };
            } else { 
                evaluating {typer.typeOf(e,Nil)} should produce [TypeException.type];
            }
        }
        i = i + 1
    }

    parseTest(
            """(\x : Bool -> (Bool -> Bool) -> Bool -> Bool.x)""",
            calc.mkAbs("x",calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkTArr(calc.mkBool,calc.mkBool),calc.mkTArr(calc.mkBool,calc.mkBool))),calc.mkVar(0,1)));
    parseTest(
            "succ 2",
            calc.mkNatLit(3));
    parseTest(
            "if iszero true then 3 else false",
            calc.mkIfThenElse(calc.mkIsZero(calc.mkTrue),calc.mkNatLit(3),calc.mkFalse));
    parseTest(
            "let x : Bool = true in (if x then 3 else 2)",
            calc.mkApp(calc.mkAbs("x",calc.mkBool,calc.mkIfThenElse(calc.mkVar(0,1),calc.mkNatLit(3),calc.mkNatLit(2))),calc.mkTrue));
    parseTest(
            """ (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """,
            calc.mkApp(calc.mkApp(calc.mkApp(calc.mkAbs("b",calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("t",calc.mkBool,calc.mkAbs("f",calc.mkBool,calc.mkApp(calc.mkApp(calc.mkVar(2,3),calc.mkVar(1,3)),calc.mkVar(0,3))))),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(0,2)))),calc.mkTrue),calc.mkFalse));

    evaluateAndTypeTest("let x : Bool = true in (if x then 3 else 2)", calc.mkNat, calc.mkNatLit(3));
    evaluateAndTypeTest("succ 0 ", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("succ pred 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if true then false else true", calc.mkBool,calc.mkFalse);
    evaluateAndTypeTest("iszero pred 0", calc.mkBool, calc.mkTrue);
    evaluateAndTypeTest("if iszero pred 0 then succ 0 else pred succ 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if iszero 0 then succ 0 else 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if iszero succ 0 then true else false",calc.mkBool,calc.mkFalse);
    evaluateAndTypeTest("0",calc.mkNat,calc.mkZero);
    evaluateAndTypeTest(""" \x:Bool. \y:Bool.x """, 
            calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(1,2)))); // the encoded boolean true over the Bool type
    evaluateAndTypeTest(""" \x:Bool. \y:Bool.y """, 
            calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(0,2)))); // the encoded boolean false over the Bool type
    evaluateAndTypeTest(""" \b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f """, 
            calc.mkTArr(calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool))),
            calc.mkAbs("b",calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("t",calc.mkBool,calc.mkAbs("f",calc.mkBool,calc.mkApp(calc.mkApp(calc.mkVar(2,3),calc.mkVar(1,3)),calc.mkVar(0,3))))));  // the conditional test
    evaluateAndTypeTest(""" (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """, calc.mkBool, calc.mkFalse); // conditional applied to false
    evaluateAndTypeTest("""(\x:Bool. if x then 0 else 1) true""",calc.mkNat,calc.mkZero);
    evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n) 0""", calc.mkNat, calc.mkNatLit(2));
    evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) ((\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n)) 0""",
            calc.mkNat, calc.mkNatLit(4));
    evaluateAndTypeTest(""" fix (\f : Nat -> Nat -> Nat. \x: Nat. if iszero x then (\y:Nat.y) else (\y:Nat. succ (f (pred x) y)))""", calc.mkTArr(calc.mkNat,calc.mkTArr(calc.mkNat,calc.mkNat)), null); 
    evaluateAndTypeTest(""" (fix (\f : Nat -> Nat -> Nat. \x: Nat. if iszero x then (\y:Nat.y) else (\y:Nat. succ (f (pred x) y)))) 3 2""", TNat, 
            calc.mkNatLit(5)); // 3 + 2 = 5 
}