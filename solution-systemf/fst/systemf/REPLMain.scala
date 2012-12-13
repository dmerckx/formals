package fst.systemf
import fst.common.Parser
import fst.common.NotSupportedException

object REPLMain {
  val tcalc = new TypeCalculator();
  val typer = new Typer(tcalc);
  val calc = new Calculus();
  val eval = new Evaluator(tcalc);
  val parser = new Parser(calc)
  
  def main(args: Array[String]): Unit = {
    while(true) {
     try {
        val in = Console.readLine("Enter an expression: ");
        if(in.isEmpty()) return;
        val t = parser.parseTerm(in)
        val ty = typer.typeOf(t);
        Console.println("Type: " + ty);
        val te = eval.eval(t);
        Console.println("Evaluated: " + te);
      } catch {
        case parser.UnknownIdentifierException(id) => Console.println("Unknown identifier: '" + id + "'");
        case parser.IncompleteParseException(failure) => Console.println(failure);
        case parser.FailedParseException(failure) => {
          Console.println("failed parse");
          Console.println(failure);
        }
        case TypeException(baseTerm, term, typ, _) => Console.println("Type error." + "base term: " + baseTerm + " \nterm: " + term + " \ntype: " + typ)
        case NotSupportedException => Console.println("Unsupported feature.")
      }
    }
  }
}