package fst.systemf

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import fst.common.Parser
import Syntax._
import org.scalatest.FunSuite
import fst.common.NotSupportedException

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {
    val calc = new Calculus();
    val parser = new Parser(calc);
    val tcalc = new TypeCalculator();
    val eval = new Evaluator(tcalc);
    val typer = new Typer(tcalc);

    var i = 0;
    def parseTest(in: String, outp: => Term) {
        test("parse expression " + i + ": '" + in + "'") {
        	try {
	        	val out = outp
	            val e = parser.parseTerm(in);
	            e should be ===(out);
        	} catch {
        	  case NotSupportedException =>
	    	    fail("Test failed: feature not supported");
        	}
        }
        i = i + 1
    }

    def evaluateAndTypeTest(inp: => String, typ: => Type, outp: => Term) {
        test("type and evaluate expression " + i) {
	    	try {
	    		val in = inp;
	    		info("expression '" + in.toString() + "'");
		    	val ty = typ;
		    	val out = outp
	            val e : Term = parser.parseTerm(in);
	            if( ty != null) {
	                typer.typeOf(e) should be ===(ty);
	                if(out != null) { eval.eval(e) should be ===(out) };
	            } else { 
	                evaluating {typer.typeOf(e)} should produce [TypeException.type];
	            }
	    	} catch {
	    	  case NotSupportedException =>
	    	    fail("Test failed: feature not supported");
	    	}
        }
        i = i + 1
    }

    //Parse expression 0 -> 4
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

    //Type and evaluate expression 5
    evaluateAndTypeTest("let x : Bool = true in (if x then 3 else 2)", calc.mkNat, calc.mkNatLit(3));
    evaluateAndTypeTest("succ 0 ", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("succ pred 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if true then false else true", calc.mkBool,calc.mkFalse);
    evaluateAndTypeTest("iszero pred 0", calc.mkBool, calc.mkTrue);
    
    //Type and evaluate expression 10
    evaluateAndTypeTest("if iszero pred 0 then succ 0 else pred succ 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if iszero 0 then succ 0 else 0", calc.mkNat, calc.mkNatLit(1));
    evaluateAndTypeTest("if iszero succ 0 then true else false",calc.mkBool,calc.mkFalse);
    evaluateAndTypeTest("0",calc.mkNat,calc.mkZero);
    evaluateAndTypeTest(""" \x:Bool. \y:Bool.x """, 
            calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(1,2)))); // the encoded boolean true over the Bool type
    
    //Type and evaluate expression 15
    evaluateAndTypeTest(""" \x:Bool. \y:Bool.y """, 
            calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(0,2)))); // the encoded boolean false over the Bool type
    evaluateAndTypeTest(""" \b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f """, 
            calc.mkTArr(calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool))),
            calc.mkAbs("b",calc.mkTArr(calc.mkBool,calc.mkTArr(calc.mkBool,calc.mkBool)),calc.mkAbs("t",calc.mkBool,calc.mkAbs("f",calc.mkBool,calc.mkApp(calc.mkApp(calc.mkVar(2,3),calc.mkVar(1,3)),calc.mkVar(0,3))))));  // the conditional test
    evaluateAndTypeTest(""" (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """, calc.mkBool, calc.mkFalse); // conditional applied to false
    evaluateAndTypeTest("""(\x:Bool. if x then 0 else 1) true""",calc.mkNat,calc.mkZero);
    evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n) 0""", calc.mkNat, calc.mkNatLit(2));
    
    //Type and evaluate expression 20
    evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) ((\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n)) 0""",
            calc.mkNat, calc.mkNatLit(4));
    evaluateAndTypeTest(""" \X. \x:X.x """, 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)),"X"), calc.mkTAbs(calc.mkAbs("x",calc.mkTVar(0,1),calc.mkVar(0,2)),"X")); // polymorphic identity
    evaluateAndTypeTest(""" ((\X. \x:X.x) [Nat]) """, calc.mkTArr(calc.mkNat,calc.mkNat), calc.mkAbs("x",calc.mkNat,calc.mkVar(0,1))); // polymorphic identity applied to Nat
    evaluateAndTypeTest(""" ((\X. \x:X.x) [Nat]) 0 """, calc.mkNat, calc.mkZero); // polymorphic identity applied to Nat applied to 0
    evaluateAndTypeTest(""" (\X. \f:X->X. (\a:X. f (f a)))""", 
            calc.mkTAll(calc.mkTArr(calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), calc.mkTAbs(calc.mkAbs("f",calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)),calc.mkAbs("a",calc.mkTVar(1,2),calc.mkApp(calc.mkVar(1,3),calc.mkApp(calc.mkVar(1,3),calc.mkVar(0,3))))),"X")); // polymorphic double
    
    //Type and evaluate expression 25
    evaluateAndTypeTest(""" ((\X. \f:X->X. (\a:X. f (f a))) [Nat]) (\n:Nat. succ n) 0""", 
            calc.mkNat, calc.mkNatLit(2));// polymorphic double applied to Nat applied to succ
    evaluateAndTypeTest(""" \X. \t:X. (\f:X. t) """, 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), 
            calc.mkTAbs(calc.mkAbs("t",calc.mkTVar(0,1),calc.mkAbs("f",calc.mkTVar(1,2),calc.mkVar(1,3))),"X")); // tru: CBool
    evaluateAndTypeTest(""" \X. \t:X. (\f:X. f) """, calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), 
            calc.mkTAbs(calc.mkAbs("t",calc.mkTVar(0,1),calc.mkAbs("f",calc.mkTVar(1,2),calc.mkVar(0,3))),"X")); // fls: CBool
    evaluateAndTypeTest(""" \b:All X. X->X->X. \X. \t:X. (\f:X. b [X] f t) """, 
            calc.mkTArr(calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"),calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X")),
            calc.mkAbs("b",calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"),calc.mkTAbs(calc.mkAbs("t",calc.mkTVar(0,2),calc.mkAbs("f",calc.mkTVar(1,3),calc.mkApp(calc.mkApp(calc.mkTApp(calc.mkVar(3,4),calc.mkTVar(2,4)),calc.mkVar(0,4)),calc.mkVar(1,4)))),"X"))); // not: CBool -> CBool
    evaluateAndTypeTest(""" ((\b:All X. X->X->X. \X. \t:X. (\f:X. b [X] f t)) (\X. \t:X. (\f:X. t))) [Bool] true false """, calc.mkBool, calc.mkFalse); // (not tru) [Bool] true false
    
    //Type and evaluate expression 30
    evaluateAndTypeTest(""" ((\X. \x:X.x) [All X. X->X->X]) (\T. \x:T. (\y:T. x)) """, 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), 
            calc.mkTAbs(calc.mkAbs("x",calc.mkTVar(0,1),calc.mkAbs("y",calc.mkTVar(1,2),calc.mkVar(1,3))),"X"));
    
    // syntactic equality
    evaluateAndTypeTest("""(\Z. \x:Z.x)""", 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)), "X"),
            calc.mkTAbs(calc.mkAbs("z",calc.mkTVar(0,1),calc.mkVar(0,2)),"X"));
    // type applications in terms and types...
    evaluateAndTypeTest("""\X.((\Z. \x:All Y. Y -> Y.true)[Nat])""", 
            calc.mkTAll(calc.mkTArr(calc.mkTAll(calc.mkTArr(calc.mkTVar(0,2), calc.mkTVar(0,2)),"Y"),calc.mkBool), "Z"),
            calc.mkTAbs(calc.mkTApp(calc.mkTAbs(calc.mkAbs("x",calc.mkTAll(calc.mkTArr(calc.mkTVar(0,3), calc.mkTVar(0,3)),"Y"),calc.mkTrue),"Z"),calc.mkNat),"X"));
    evaluateAndTypeTest("""(\X. \Y. \Z. \x:Z.x)[Nat][Bool]""", 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)), "Z"),
            calc.mkTAbs(calc.mkAbs("z",calc.mkTVar(0,1),calc.mkVar(0,2)),"Z"));
    evaluateAndTypeTest("""((\b:Bool.\X. \Y. \Z. \x:Z.b) true)[Nat][Bool]""", 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkBool), "Z"),
            calc.mkTAbs(calc.mkAbs("z",calc.mkTVar(0,1),calc.mkTrue),"Z"));
    
    //Type and evaluate expression 35
    evaluateAndTypeTest("""\x:Bool.(((\b:Bool.\X. \Y. \Z. \x:Z.b) true)[Nat][Bool])""", 
            calc.mkTArr(calc.mkBool, calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkBool), "Z")),
            calc.mkAbs("x",calc.mkBool,calc.mkTApp(calc.mkTApp(calc.mkApp(calc.mkAbs("b",calc.mkBool,calc.mkTAbs(calc.mkTAbs(calc.mkTAbs(calc.mkAbs("z",calc.mkTVar(0,5),calc.mkVar(4,6)),"Z"),"Y"),"X")),calc.mkTrue),calc.mkNat),calc.mkBool)));
    
    // fixp primitive
    evaluateAndTypeTest(""" fixp [Nat -> Nat -> Nat] (\f : Nat -> Nat -> Nat. \x: Nat. if iszero x then (\y:Nat.y) else (\y:Nat. succ (f (pred x) y)))""",
                        calc.mkTArr(calc.mkNat,calc.mkTArr(calc.mkNat,calc.mkNat)),
                        null); 
    evaluateAndTypeTest(""" (fixp [Nat -> Nat -> Nat] (\f : Nat -> Nat -> Nat. \x: Nat. if iszero x then (\y:Nat.y) else (\y:Nat. succ (f (pred x) y)))) 3 2""",
                        calc.mkNat, 
                        calc.mkNatLit(5)); // 3 + 2 = 5

    // church data types
    //cfalse
    evaluateAndTypeTest(calc.cfalseDef, 
            calc.mkCBool(0),
            null);
    //ctrue
    evaluateAndTypeTest(calc.ctrueDef, 
            calc.mkCBool(0),
            null);
    //cand
    
    
    //Type and evaluate expression 40
    evaluateAndTypeTest(
        """let cfalse : CBool = """ + calc.cfalseDef + """ in """ +
        """let ctrue : CBool = """ + calc.ctrueDef + """ in """ +
        calc.candDef, 
            calc.mkTArr(calc.mkCBool(0),calc.mkTArr(calc.mkCBool(0),calc.mkCBool(0))),
            null);
    // test cand
    evaluateAndTypeTest(
        """let cfalse : CBool = """ + calc.cfalseDef + """ in """ +
        """let ctrue : CBool = """ + calc.ctrueDef + """ in """ +
        """let cand : CBool -> CBool -> CBool = """ + calc.candDef + """ in """ +
        """cand cfalse ctrue""", 
            calc.mkCBool(0),
            calc.mkTAbs(calc.mkAbs("x",calc.mkTVar(0,1),calc.mkAbs("y",calc.mkTVar(1,2), calc.mkVar(0,3))),"X"));
    //boolToCBool
    evaluateAndTypeTest(calc.boolToCBool,
        calc.mkTArr(calc.mkBool,calc.mkCBool(0)),
        null);
    //cboolToBool
    evaluateAndTypeTest(calc.cboolToBool,
        calc.mkTArr(calc.mkCBool(0),calc.mkBool),
        null);
    // test boolToCBool and cboolToBool
    evaluateAndTypeTest(
        """let boolToCBool : Bool -> CBool = """ + calc.boolToCBool + """ in """ +
        """let cboolToBool : CBool -> Bool  = """ + calc.cboolToBool + """ in """ +
        """cboolToBool (boolToCBool false)""", 
            calc.mkBool,
            calc.mkFalse);
    
    //Type and evaluate expression 45
    evaluateAndTypeTest(
        """let boolToCBool : Bool -> CBool = """ + calc.boolToCBool + """ in """ +
        """let cboolToBool : CBool -> Bool = """ + calc.cboolToBool + """ in """ +
        """cboolToBool (boolToCBool true)""", 
            calc.mkBool,
            calc.mkTrue);
    evaluateAndTypeTest(
        """let cboolToBool : CBool -> Bool = """ + calc.cboolToBool + """ in """ +
        """let ctrue : CBool = """ + calc.ctrueDef + """ in """ +
        """cboolToBool ctrue""", 
            calc.mkBool,
            calc.mkTrue);

    // czero
    evaluateAndTypeTest(calc.czeroDef,
        calc.mkCNat(0), null);
    // csucc
    evaluateAndTypeTest(calc.csuccDef,
        calc.mkTArr(calc.mkCNat(0),calc.mkCNat(0)), null);
    
    //natToCNat
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        calc.natToCNatDef,
        calc.mkTArr(calc.mkNat,calc.mkCNat(0)),
        null);
    //cnatToNat
    
   
    //Type and evaluate expression 50
    evaluateAndTypeTest(
        calc.cnatToNatDef,
        calc.mkTArr(calc.mkCNat(0),calc.mkNat),
        null);
    //test natToCNat and cnatToNat
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """cnatToNat (natToCNat 10)""",
        calc.mkNat,
        calc.mkNatLit(10));
    //test cplus
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        calc.cplusDef,
        calc.mkTArr(calc.mkCNat(0),calc.mkTArr(calc.mkCNat(0), calc.mkCNat(0))),
        null);
    //test cplus
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """cnatToNat (cplus (natToCNat 10) (natToCNat 20))""",
        calc.mkNat,
        calc.mkNatLit(30));
    //test ctimes
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        calc.ctimesDef,
        calc.mkTArr(calc.mkCNat(0),calc.mkTArr(calc.mkCNat(0), calc.mkCNat(0))),
        null);
    //test ctimes
    
    
    
    //Type and evaluate expression 55
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let ctimes : CNat -> CNat -> CNat = """ + calc.ctimesDef + """ in """ +
        """cnatToNat (ctimes (natToCNat 5) (natToCNat 6))""",
        calc.mkNat,
        calc.mkNatLit(30));
    //test cexp
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let ctimes : CNat -> CNat -> CNat = """ + calc.ctimesDef + """ in """ +
        calc.cexpDef,
        parser.parseType("CNat -> CNat -> CNat"),
        null);
    //test cexp
    evaluateAndTypeTest(
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let ctimes : CNat -> CNat -> CNat = """ + calc.ctimesDef + """ in """ +
        """let cexp : CNat -> CNat -> CNat = """ + calc.cexpDef + """ in """ +
        """cnatToNat (cexp (natToCNat 4) (natToCNat 3))""",
        calc.mkNat,
        calc.mkNatLit(64));

    // Church Maybe
    // test cnothing
    evaluateAndTypeTest(calc.cnothingDef, 
        parser.parseType("All X. CMaybe X"), null);
    
    // test cjust
    evaluateAndTypeTest(calc.cjustDef,
        parser.parseType("All X. X -> CMaybe X"), null);
    // test cmaybe
    
    
    
    //Type and evaluate expression 60
    evaluateAndTypeTest(calc.cmaybeDef, calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkCMaybe(calc.mkTVar(0,1),1),calc.mkTVar(0,1))),"Y"), null);
    // test cmap
    evaluateAndTypeTest(
        """let cnothing : All X. CMaybe X = """ + calc.cnothingDef + """ in """ +
        """let cjust : All X. X -> CMaybe X = """ + calc.cjustDef + """ in """ +
        calc.cmapDef,
        parser.parseType("All X. All Y. (X -> Y) -> CMaybe X -> CMaybe Y"), null);
    
    // test cpred
    evaluateAndTypeTest(
        """let cnothing : All X. CMaybe X = """ + calc.cnothingDef + """ in """ +
        """let cjust : All X. X -> CMaybe X = """ + calc.cjustDef + """ in """ +
        """let cmaybe : All X. X -> CMaybe X -> X = """ + calc.cmaybeDef + """ in """ +
        """let cmap : All X. All Y. (X -> Y) -> CMaybe X -> CMaybe Y = """ + calc.cmapDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        calc.cpredDef,
        parser.parseType("CNat -> CNat"), null);
    evaluateAndTypeTest(
        """let cnothing : All X. CMaybe X = """ + calc.cnothingDef + """ in """ +
        """let cjust : All X. X -> CMaybe X = """ + calc.cjustDef + """ in """ +
        """let cmaybe : All X. X -> CMaybe X -> X = """ + calc.cmaybeDef + """ in """ +
        """let cmap : All X. All Y. (X -> Y) -> CMaybe X -> CMaybe Y = """ + calc.cmapDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let cpred : CNat -> CNat = """ + calc.cpredDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """cnatToNat (cpred czero)""",
        calc.mkNat, calc.mkZero);
    evaluateAndTypeTest(
        """let cnothing : All X. CMaybe X = """ + calc.cnothingDef + """ in """ +
        """let cjust : All X. X -> CMaybe X = """ + calc.cjustDef + """ in """ +
        """let cmaybe : All X. X -> CMaybe X -> X = """ + calc.cmaybeDef + """ in """ +
        """let cmap : All X. All Y. (X -> Y) -> CMaybe X -> CMaybe Y = """ + calc.cmapDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let cpred : CNat -> CNat = """ + calc.cpredDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """cnatToNat (cpred (csucc czero))""",
        calc.mkNat, calc.mkZero);
    
    
    //Type and evaluate expression 65
    evaluateAndTypeTest(
        """let cnothing : All X. CMaybe X = """ + calc.cnothingDef + """ in """ +
        """let cjust : All X. X -> CMaybe X = """ + calc.cjustDef + """ in """ +
        """let cmaybe : All X. X -> CMaybe X -> X = """ + calc.cmaybeDef + """ in """ +
        """let cmap : All X. All Y. (X -> Y) -> CMaybe X -> CMaybe Y = """ + calc.cmapDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let cpred : CNat -> CNat = """ + calc.cpredDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """cnatToNat (cpred (csucc (csucc czero)))""",
        calc.mkNat, calc.mkNatLit(1));

    
    // cisnil
    evaluateAndTypeTest(calc.cisnilDef,
            parser.parseType("All X. CList X -> Bool"),
            null);
    // cnil
    evaluateAndTypeTest(calc.cnilDef, 
            parser.parseType("All X. CList X"),
            null);
    // ccons
    evaluateAndTypeTest(calc.cconsDef, 
            parser.parseType("All X. X -> CList X -> CList X"),
            null);
    // test clist stuff
    evaluateAndTypeTest(
        """let cisnil : All X. CList X -> Bool = """ + calc.cisnilDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """cisnil [Bool] (cnil [Bool])""",
        calc.mkBool,
        calc.mkTrue);
    
    
    //Type and evaluate expression 70
    evaluateAndTypeTest(
        """let cisnil : All X. CList X -> Bool = """ + calc.cisnilDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """cisnil [Bool] (ccons [Bool] true (cnil [Bool]))""",
        calc.mkBool,
        calc.mkFalse);
    
    
    // Church Pairs
    // ccomma
    evaluateAndTypeTest(
        calc.ccommaDef,
        parser.parseType("""All X. All Y. X -> Y -> CPair X Y"""),
        null);
    // cfst
    evaluateAndTypeTest(
        calc.cfstDef,
        parser.parseType("""All X. All Y. CPair X Y -> X"""),
        null);
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """cfst [Bool] [Nat] (ccomma [Bool] [Nat] true 3)""",
        calc.mkBool,
        calc.mkTrue);
    // csnd
    evaluateAndTypeTest(
        calc.csndDef,
        parser.parseType("""All X. All Y. CPair X Y -> Y"""),
        null);
    
    
    //Type and evaluate expression 75
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """csnd [Bool] [Nat] (ccomma [Bool] [Nat] true 3)""",
        calc.mkNat,
        calc.mkNatLit(3));
    
    // fibonacci numbers
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        calc.cfibDef,
        parser.parseType("""CNat -> CNat"""),
        null);
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib czero)""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(0));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib (csucc czero))""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(1));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib (csucc (csucc czero)))""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(1));
    
    
    
    //Type and evaluate expression 80
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib (csucc (csucc (csucc (csucc czero)))))""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(3));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib (csucc (csucc (csucc (csucc (csucc czero))))))""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(5));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let czero : CNat = """ + calc.czeroDef + """ in """ +
        """let csucc : CNat -> CNat = """ + calc.csuccDef + """ in """ +
        """let natToCNat : Nat -> CNat = """ + calc.natToCNatDef + """ in """ +
        """let cnatToNat : CNat -> Nat = """ + calc.cnatToNatDef + """ in """ +
        """let cplus : CNat -> CNat -> CNat = """ + calc.cplusDef + """ in """ +
        """let cfib : CNat -> CNat = """ + calc.cfibDef + """ in """ +
        """cnatToNat (cfib (csucc (csucc (csucc (csucc (csucc (csucc (csucc (csucc (csucc czero))))))))))""",
        parser.parseType("""Nat"""),
        calc.mkNatLit(34));
    
    
    // insertion sort
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        calc.cinsertDef,
        parser.parseType("""All X. (X -> X -> Bool) -> CList X -> X -> CList X"""),
        null);
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cinsert [Nat] lt (cnil [Nat]) 5""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 5 (cnil [Nat])""")));
    
    //Type and evaluate expression 85
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cinsert [Nat] lt (ccons [Nat] 10 (cnil [Nat])) 5""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat]))""")));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cinsert [Nat] lt (ccons [Nat] 5 (cnil [Nat])) 10""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat]))""")));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cinsert [Nat] lt (ccons [Nat] 3 (ccons [Nat] 10 (cnil [Nat]))) 5""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat])))""")));
    // cisort
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        calc.cisortDef,
        parser.parseType("""All X. (X -> X -> Bool) -> CList X -> CList X"""),
        null);
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let cisort : All X. (X -> X -> Bool) -> CList X -> CList X = """ + calc.cisortDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cisort [Nat] lt (ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat]))))""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat])))""")));
    
    
    //Type and evaluate expression 90
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let cisort : All X. (X -> X -> Bool) -> CList X -> CList X = """ + calc.cisortDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cisort [Nat] lt (ccons [Nat] 10 (ccons [Nat] 5 (ccons [Nat] 3 (cnil [Nat]))))""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat])))""")));
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let cisort : All X. (X -> X -> Bool) -> CList X -> CList X = """ + calc.cisortDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cisort [Nat] lt (ccons [Nat] 5 (ccons [Nat] 10 (ccons [Nat] 3 (cnil [Nat]))))""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat])))""")));
}