import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap/dist/js/bootstrap.js';
import $ from 'jquery/dist/jquery.slim.js';
import * as editor from './editor.js';

import * as wasm from "wasm-layer";

document.querySelector('#run-btn').addEventListener('click', e => {
    var input = editor.inputEditor.doc.getValue();

    try {
        var result = wasm.parse_input(input);

        if (result.get_kind() == wasm.InputKind.StatementSet) {
            let is_consistent = result.is_consistent();

            setAlert(
                'success',
                'Input: Statement Set',
                `Statement set is${is_consistent.is_consistent ? ' ' : ' not '}consistent.`
            );
        } else if (result.get_kind() == wasm.InputKind.Argument) {
            let is_valid = result.is_valid();

            setAlert(
                'success',
                'Input: Argument',
                `Argument is${is_valid.is_valid ? ' ' : ' not '}valid.`
            );
        } else if (result.get_kind() == wasm.InputKind.Statement) {
            let is_contradiction = result.is_contradiction();
            let is_tautology = result.is_tautology();
            let is_contingency = result.is_contingency();

            setAlert(
                'success',
                'Input: Statement',
                `Statement is a ${
                (is_contradiction.is_contradiction && 'contradiction') ||
                (is_tautology.is_tautology && 'tautology') ||
                'contingency'
                }.`
            )
        }
    }
    catch (e) {
        setAlert('error', 'Parse error', e.message);
    }
});

function setAlert(type, heading, message) {
    var classes = {
        'success': 'alert-success',
        'error': 'alert-danger',
    };

    if (!Object.keys(classes).includes(type)) {
        throw "invalid alert type";
    }

    Object.values(classes).forEach(v => $('#alert').removeClass(v));

    $('#alert').addClass(classes[type]);

    $('#alert-heading').text(heading);

    $('#alert-message').text(message);

    $('#alert').attr('hidden', false);
}