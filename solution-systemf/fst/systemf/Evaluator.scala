package fst.systemf

import Syntax._;

/**
 * @author VUL IN AUB.
 */
class Evaluator(tcalc: TypeCalculator) {
    def termSubstTop(s:Term, t:Term): Term = null
    def tytermSubstTop(s:Type, t:Term): Term = null
    
    case object NoRuleApplies extends Exception;

    // one step evaluation
    def eval1(t:Term): Term = null

    // evaluation to normal form
    def eval(t:Term): Term = null
}