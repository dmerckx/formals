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
    
    evaluateAndTypeTest(""" (\X. \f:X->X. (\a:X. f (f a)))""", 
            calc.mkTAll(calc.mkTArr(calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), calc.mkTAbs(calc.mkAbs("f",calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1)),calc.mkAbs("a",calc.mkTVar(1,2),calc.mkApp(calc.mkVar(1,3),calc.mkApp(calc.mkVar(1,3),calc.mkVar(0,3))))),"X")); // polymorphic double
 
    
    
}