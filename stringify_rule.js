import { tokens } from './tokens';

export default function ruleNameToLabel(ruleName) {
    if (ruleName == "QuantifierExchange") {
        return "QE";
    } else if (ruleName == "ExistentialQuantifier") {
        return "EQ";
    } else if (ruleName == "UniversalQuantifier") {
        return "UQ";
    } else if (ruleName == "DoubleNegation") {
        return tokens.negation.repeat(2);
    } else if (ruleName == "Conjunction") {
        return tokens.conjunction;
    } else if (ruleName == "NegationOfConditional") {
        return tokens.negation.concat(tokens.conditional);
    } else if (ruleName == "NegationOfDisjunction") {
        return tokens.negation.concat(tokens.disjunction);
    } else if (ruleName == "Conditional") {
        return tokens.conditional;
    } else if (ruleName == "NegationOfConjunction") {
        return tokens.negation.concat(tokens.conjunction);
    } else if (ruleName == "Disjunction") {
        return tokens.disjunction;
    } else {
        throw "ruleName was called with invalid rule";
    }
}