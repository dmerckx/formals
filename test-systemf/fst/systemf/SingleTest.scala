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
    
    
   
    evaluateAndTypeTest(""" ((\b:All X. X->X->X. \X. \t:X. (\f:X. b [X] f t)) (\X. \t:X. (\f:X. t))) [Bool] true false """, calc.mkBool, calc.mkFalse); // (not tru) [Bool] true false
    
    
}