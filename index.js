import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap/dist/js/bootstrap.js';
import $ from 'jquery/dist/jquery.slim.js';
import { inputEditor } from './editor';
import TruthTree from './truth_tree';

import * as wasm from "wasm-layer";

document.querySelector('#run-btn').addEventListener('click', e => {
    var input = inputEditor.doc.getValue();

    var result;

    try {
        result = wasm.parse_input(input);
    }
    catch (e) {
        setAlert('error', 'Parse error', e.message);
        return;
    }

    if (result.get_kind() == wasm.InputKind.StatementSet) {
        let is_consistent = result.is_consistent();

        setAlert(
            'success',
            'Statement Set',
            `Statement set is${is_consistent.is_consistent ? ' ' : ' not '}consistent.`
        );

        console.log(is_consistent.truth_tree);

        var truth_tree = new TruthTree();
        truth_tree.render(is_consistent.truth_tree);
    } else if (result.get_kind() == wasm.InputKind.Argument) {
        let is_valid = result.is_valid();

        setAlert(
            'success',
            'Argument',
            `Argument is${is_valid.is_valid ? ' ' : ' not '}valid.`
        );

        var truth_tree = new TruthTree();
        truth_tree.render(is_valid.truth_tree);
    } else if (result.get_kind() == wasm.InputKind.Statement) {
        let is_contradiction = result.is_contradiction();
        let is_tautology = result.is_tautology();
        //let is_contingency = result.is_contingency();

        setAlert(
            'success',
            'Statement',
            `Statement is a ${
            (is_contradiction.is_contradiction && 'contradiction') ||
            (is_tautology.is_tautology && 'tautology') ||
            'contingency'
            }.`
        )

        //var truth_tree = new truth_tree_renderer.TruthTree(canvas, is_contradiction.truth_tree);
        //truth_tree.render();
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