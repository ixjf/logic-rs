import { tokens } from './tokens.js';

var map = {
    '.conclusion-ind-token': tokens.conclusionIndicator,
    '.subscript-zero-token': tokens.subscriptNumber0,
    '.subscript-one-token': tokens.subscriptNumber1,
    '.subscript-two-token': tokens.subscriptNumber2,
    '.subscript-three-token': tokens.subscriptNumber3,
    '.subscript-four-token': tokens.subscriptNumber4,
    '.subscript-five-token': tokens.subscriptNumber5,
    '.subscript-six-token': tokens.subscriptNumber6,
    '.subscript-seven-token': tokens.subscriptNumber7,
    '.subscript-eight-token': tokens.subscriptNumber8,
    '.subscript-nine-token': tokens.subscriptNumber9,
    '.superscript-zero-token': tokens.superscriptNumber0,
    '.superscript-one-token': tokens.superscriptNumber1,
    '.superscript-two-token': tokens.superscriptNumber2,
    '.superscript-three-token': tokens.superscriptNumber3,
    '.superscript-four-token': tokens.superscriptNumber4,
    '.superscript-five-token': tokens.superscriptNumber5,
    '.superscript-six-token': tokens.superscriptNumber6,
    '.superscript-seven-token': tokens.superscriptNumber7,
    '.superscript-eight-token': tokens.superscriptNumber8,
    '.superscript-nine-token': tokens.superscriptNumber9,
    '.universal-token': tokens.universalQuantifier,
    '.existential-token': tokens.existentialQuantifier,
    '.conjunction-token': tokens.conjunction,
    '.negation-token': tokens.negation,
    '.disjunction-token': tokens.disjunction,
    '.conditional-token': tokens.conditional,
    '.statement-set-opening-token': tokens.statementSetOpening,
    '.statement-set-closing-token': tokens.statementSetClosing,
    '.grouper-opening-token': tokens.grouperOpening,
    '.grouper-closing-token': tokens.grouperClosing,
};

export function mapHtmlTokens() {
    Object.entries(map).forEach(v => {
        var className = v[0];
        var token = v[1];

        document.querySelectorAll(className).forEach(e => {
            e.innerHTML = token;
        });
    });
}