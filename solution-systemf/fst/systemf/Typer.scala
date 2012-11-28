package fst.systemf

import Syntax._

case object TypeException extends Exception;

/**
 * @author VUL IN AUB.
 */
class Typer(tcalc: TypeCalculator) {
      def typeOf(t : Term) : Type = null;
}