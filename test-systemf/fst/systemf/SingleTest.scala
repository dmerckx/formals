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
    
    def print(t:Term, i:Int){
      t match{
       	case Zero => println(sp(i) + "Zero");
        case Succ(t2) =>
          	println(sp(i) + "Succ(")
          	print(t2,i+2);
          	println(sp(i) + ")");
        case Pred(t2) => 
          	println(sp(i) + "Pred(")
          	println(print(t2,i+2));
          	println(sp(i) + ")");
        case IsZero(t2) => 
          	println(sp(i) + "IsZero(")
          	print(t2,i+2);
          	println(sp(i) + ")");
        case True => println(sp(i) + "True");
        case False => println(sp(i) + "False");
        case If(t1, t2, t3) => 
          	println(sp(i) + "If(")
          	print(t2,i+2);
          	println(sp(i) + "){");
          	print(t2,i+2);
          	println(sp(i) + "} else {");   
          	print(t3,i+2);  
          	println(sp(i) + "}");   
        case App(t1, t2) =>  
          	println(sp(i) + "App(")
          	print(t1,i+2);
          	println(sp(i) + ","); 
          	print(t2,i+2);
          	println(sp(i) + ")"); 
        case TApp(t1, t2) => 
          	println(sp(i) + "TApp(");
          	print(t1,i+2);
          	println(sp(i) + ","); 
          	println(sp(i) + t2.toString());
          	println(sp(i) + ")"); 
        case Var(i, n) => 
          	println(sp(i) + i + " - " + t.toString());
        case Abs(nh, ty, t1) => 
          	println(sp(i) + "Abs(");
          	println(sp(i) + nh);
          	println(sp(i) + ","); 
          	println(sp(i) + ty.toString());
          	println(sp(i) + ","); 
          	print(t1, i+2); 
          	println(sp(i) + ")");
        case TAbs(s, t) => 
          	println(sp(i) + "TAbs(");
          	println(s);
          	println(sp(i) + ","); 
          	print(t, i+2); 
          	println(sp(i) + ")");
      }
    }
    
    def sp(i:Int):String = {
      var ret = "";
      var j = 0;
      
      while(j < i){
        ret += " ";
        j = j+1;
      }
      
      return ret;
    }
    
    
    evaluateAndTypeTest(""" ((\X. \x:X.x) [All X. X->X->X]) (\T. \x:T. (\y:T. x)) """, 
            calc.mkTAll(calc.mkTArr(calc.mkTVar(0,1),calc.mkTArr(calc.mkTVar(0,1),calc.mkTVar(0,1))),"X"), 
            calc.mkTAbs(calc.mkAbs("x",calc.mkTVar(0,1),calc.mkAbs("y",calc.mkTVar(1,2),calc.mkVar(1,3))),"X"));
    
    
}