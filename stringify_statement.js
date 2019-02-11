import { tokens } from './tokens';

export default function statementIntoText(statement) {
    if (statement["Simple"]) {
        return simpleStatementIntoText(statement["Simple"]);
    } else if (statement["Singular"]) {
        return singularStatementIntoText(statement["Singular"]);
    } else if (statement["LogicalConjunction"]) {
        return logicalConjunctionStatementIntoText(statement["LogicalConjunction"]);
    } else if (statement["LogicalNegation"]) {
        return logicalNegationStatementIntoText(statement["LogicalNegation"]);
    } else if (statement["LogicalDisjunction"]) {
        return logicalDisjunctionStatementIntoText(statement["LogicalDisjunction"]);
    } else if (statement["LogicalConditional"]) {
        return logicalConditionalStatementIntoText(statement["LogicalConditional"]);
    } else if (statement["Existential"]) {
        return existentialStatementIntoText(statement["Existential"]);
    } else if (statement["Universal"]) {
        return universalStatementIntoText(statement["Universal"]);
    } else {
        throw "statement is not of known type";
    }
}

function subscriptNumberIntoText(number) {
    return Array.from(number.toString()).map(v => {
        if (v == '0') {
            return tokens.subscriptNumber0;
        } else if (v == '1') {
            return tokens.subscriptNumber1;
        } else if (v == '2') {
            return tokens.subscriptNumber2;
        } else if (v == '3') {
            return tokens.subscriptNumber3;
        } else if (v == '4') {
            return tokens.subscriptNumber4;
        } else if (v == '5') {
            return tokens.subscriptNumber5;
        } else if (v == '6') {
            return tokens.subscriptNumber6;
        } else if (v == '7') {
            return tokens.subscriptNumber7;
        } else if (v == '8') {
            return tokens.subscriptNumber8;
        } else if (v == '9') {
            return tokens.subscriptNumber9;
        }
    });
}

function simpleStatementIntoText(statement) {
    var letter = statement[0];
    var subscript = statement[1] != null ? subscriptNumberIntoText(statement[1]) : '';

    return [
        letter,
        subscript
    ].join('');
}

function superscriptNumberIntoText(number) {
    return Array.from(number.toString()).map(v => {
        if (v == '0') {
            return tokens.superscriptNumber0;
        } else if (v == '1') {
            return tokens.superscriptNumber1;
        } else if (v == '2') {
            return tokens.superscriptNumber2;
        } else if (v == '3') {
            return tokens.superscriptNumber3;
        } else if (v == '4') {
            return tokens.superscriptNumber4;
        } else if (v == '5') {
            return tokens.superscriptNumber5;
        } else if (v == '6') {
            return tokens.superscriptNumber6;
        } else if (v == '7') {
            return tokens.superscriptNumber7;
        } else if (v == '8') {
            return tokens.superscriptNumber8;
        } else if (v == '9') {
            return tokens.superscriptNumber9;
        }
    });
}

function predicateLetterIntoText(pred_letter) {
    let letter = pred_letter[0];
    let subscript = pred_letter[1] != null ? subscriptNumberIntoText(pred_letter[1]) : '';
    let degree = superscriptNumberIntoText(pred_letter[2]);

    return [
        letter,
        subscript,
        degree
    ].join('');
}

function singularTermIntoText(term) {
    var letter = term[0];
    var subscript = term[1] != null ? subscriptNumberIntoText(term[1]) : '';

    return [
        letter,
        subscript
    ].join('');
}

function singularStatementIntoText(statement) {
    let pred_letter = predicateLetterIntoText(statement[0]);
    let terms = statement[1].map(v => singularTermIntoText(v));

    return [
        pred_letter,
        terms.join('')
    ].join('');
}

function logicalConjunctionStatementIntoText(statement) {
    var lst = statementIntoText(statement[0]);
    var rst = statementIntoText(statement[1]);

    return [
        tokens.grouperOpening,
        lst,
        ' ',
        tokens.conjunction,
        ' ',
        rst,
        tokens.grouperClosing,
    ].join('');
}

function logicalNegationStatementIntoText(rst) {
    rst = statementIntoText(rst);

    return [
        tokens.negation,
        rst
    ].join('');
}

function logicalDisjunctionStatementIntoText(statement) {
    var lst = statementIntoText(statement[0]);
    var rst = statementIntoText(statement[1]);

    return [
        tokens.grouperOpening,
        lst,
        ' ',
        tokens.disjunction,
        ' ',
        rst,
        tokens.grouperClosing
    ].join('');
}

function logicalConditionalStatementIntoText(statement) {
    var lst = statementIntoText(statement[0]);
    var rst = statementIntoText(statement[1]);

    return [
        tokens.grouperOpening,
        lst,
        ' ',
        tokens.conditional,
        ' ',
        rst,
        tokens.grouperClosing
    ].join('');
}

function variableIntoText(variable) {
    let letter = variable[0];
    let subscript = variable[1] != null ? subscriptNumberIntoText(variable[1]) : '';

    return [
        letter,
        subscript
    ].join('');
}

function formulaIntoText(formula) {
    if (formula["Statement"]) {
        return statementIntoText(formula["Statement"]);
    } else if (formula["Predicate"]) {
        return predicateIntoText(formula["Predicate"]);
    } else if (formula["Conjunction"]) {
        return conjunctionFormulaIntoText(formula["Conjunction"]);
    } else if (formula["Negation"]) {
        return negationFormulaIntoText(formula["Negation"]);
    } else if (formula["Disjunction"]) {
        return disjunctionFormulaIntoText(formula["Disjunction"]);
    } else if (formula["Conditional"]) {
        return conditionalFormulaIntoText(formula["Conditional"]);
    } else {
        throw "formula is not of known type";
    }
}

function termIntoText(term) {
    if (term["Variable"]) {
        return variableIntoText(term["Variable"]);
    } else if (term["SingularTerm"]) {
        return singularTermIntoText(term["SingularTerm"]);
    } else {
        throw "term is not of known type";
    }
}

function predicateIntoText(pred) {
    var pred_letter = predicateLetterIntoText(pred[0]);
    var terms = pred[1].map(v => termIntoText(v));

    return [
        pred_letter,
        terms.join('')
    ].join('');
}

function conjunctionFormulaIntoText(formula) {
    let lformula = formulaIntoText(formula[0]);
    let rformula = formulaIntoText(formula[1]);

    return [
        tokens.grouperOpening,
        lformula,
        ' ',
        tokens.conjunction,
        ' ',
        rformula,
        tokens.grouperClosing
    ].join('');
}

function negationFormulaIntoText(rformula) {
    rformula = formulaIntoText(rformula);

    return [
        tokens.negation,
        rformula,
    ].join('');
}

function disjunctionFormulaIntoText(formula) {
    let lformula = formulaIntoText(formula[0]);
    let rformula = formulaIntoText(formula[1]);

    return [
        tokens.grouperOpening,
        lformula,
        ' ',
        tokens.disjunction,
        ' ',
        rformula,
        tokens.grouperClosing
    ].join('');
}

function conditionalFormulaIntoText(formula) {
    let lformula = formulaIntoText(formula[0]);
    let rformula = formulaIntoText(formula[1]);

    return [
        tokens.grouperOpening,
        lformula,
        ' ',
        tokens.conditional,
        ' ',
        rformula,
        tokens.grouperClosing
    ].join('');
}

function existentialStatementIntoText(statement) {
    var variable = variableIntoText(statement[0]);
    var formula = formulaIntoText(statement[1]);

    return [
        tokens.grouperOpening,
        tokens.existentialQuantifier,
        variable,
        tokens.grouperClosing,
        formula
    ].join('');
}

function universalStatementIntoText(statement) {
    var variable = variableIntoText(statement[0]);
    var formula = formulaIntoText(statement[1]);

    return [
        tokens.grouperOpening,
        tokens.universalQuantifier,
        variable,
        tokens.grouperClosing,
        formula
    ].join('');
}