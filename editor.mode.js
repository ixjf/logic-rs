import CodeMirror from 'codemirror/lib/codemirror.js';
import 'codemirror/addon/mode/simple.js';
import { tokens } from './tokens.js';

CodeMirror.defineSimpleMode("rpl", {
    start: [
        { regex: /([(])/, token: "grouper", indent: true },
        { regex: /([)])/, token: "grouper", dedent: true },

        { regex: /([{])/, token: "statement-set", indent: true },
        { regex: /([}])/, token: "statement-set", dedent: true },

        { regex: /([,])/, token: "statement-separator" },

        { regex: new RegExp(tokens.conclusionIndicator), token: "conclusion-indicator" },

        { regex: /([A-Z])/, token: "identifier" },

        // Matches any subscript whatsoever - we're not the police
        { regex: /([\u2080-\u2089])+/, token: "identifier-sub" },

        { regex: /([&])/, token: "connective" },

        { regex: /([~])/, token: "connective" },

        { regex: new RegExp(tokens.disjunction), token: "connective" },

        { regex: new RegExp(tokens.conditional), token: "connective" },

        { regex: /([a-w])/, token: "singular-term" },

        { regex: /([\u2074-\u2079]|[\u00B2-\u00B3]|[\u00B9]|[\u2070])+/, token: "degree" },

        { regex: new RegExp(tokens.existentialQuantifier), token: "quantifier" },
        { regex: new RegExp(tokens.universalQuantifier), token: "quantifier" },

        { regex: /([x-z])/, token: "variable" },
    ],
    meta: {
        electricInput: /^\s*([}]|[)])$/ // Triggers reindentation if } or ) appear in a newline
        // i.e. {
        //      -- (A & B),
        //      -- }
        //      <<<< ^ is dedented
        //      }
    }
});