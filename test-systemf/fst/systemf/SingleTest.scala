package fst.systemf

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import fst.common.Parser
import Syntax._
import org.scalatest.FunSuite
import fst.common.NotSupportedException

@RunWith(classOf[JUnitRunner])
class SingleTest extends FunSuite with ShouldMatchers {
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
		    	
		    	println(in);
		    	println("After parsing: " + e);
		    	
		    	println("");
		    	println("Expected type: " + ty);
		    	println("Type found: " + typer.typeOf(e));
		    	println("Expected eval: " + out);
		    	
		    	println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
				
		    	println("Eval found: " + eval.eval(e));
		    	
				println("");
				println("----------------------------");
				println("");
		    	
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
    
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """cinsert [Nat] lt (ccons [Nat] 0 (ccons [Nat] 1 (cnil [Nat]))) 2""",
        parser.parseType("""CList Nat"""),
        eval.eval(parser.parseTerm(
            """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
            """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
            """ccons [Nat] 0 (ccons [Nat] 1 (ccons [Nat] 2 (cnil [Nat])))""")));
    
    /*
    evaluateAndTypeTest(
        """let ccomma : All X. All Y. X -> Y -> CPair X Y = """ + calc.ccommaDef + """ in """ +
        """let cfst : All X. All Y. CPair X Y -> X = """ + calc.cfstDef + """ in """ +
        """let csnd : All X. All Y. CPair X Y -> Y = """ + calc.csndDef + """ in """ +
        """let cnil : All X. CList X = """ + calc.cnilDef + """ in """ +
        """let ccons : All X. X -> CList X -> CList X = """ + calc.cconsDef + """ in """ +
        """let cinsert : All X. (X -> X -> Bool) -> CList X -> X -> CList X = """ + calc.cinsertDef + """ in """ +
        """let lt : Nat -> Nat -> Bool = fixp [Nat -> Nat -> Bool] (\rec : Nat -> Nat -> Bool. \x : Nat. \y : Nat. if iszero y then false else (if iszero x then true else rec (pred x) (pred y))) in """ +
        """if csnd [Bool][Bool] (ccomma[Bool][Bool] false true) then true else false""",
        parser.parseType("""Bool"""),
        eval.eval(parser.parseTerm("""true""")));
        */
    
    /*
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
            """ccons [Nat] 3 (ccons [Nat] 5 (ccons [Nat] 10 (cnil [Nat])))""")));*/
}