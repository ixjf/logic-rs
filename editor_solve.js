import * as logic_wasm from "wasm-layer";
import TruthTree from './truth_tree.js';

export function solve(input) {
    var parsedInput = null;

    try {
        parsedInput = logic_wasm.parse_input(input);
    }
    catch (e) {
        _showParseError(e);
        return;
    }

    _hideAnyParseError();

    if (parsedInput.get_kind() == logic_wasm.InputKind.StatementSet) {
        var isConsistent = parsedInput.is_consistent();

        document.getElementById('output-tab-container').innerHTML = _computeHtmlOutput({
            message: `Statement set is${isConsistent.is_consistent ? ' ' : ' not '}consistent.`,
            trees: [
                {
                    id: 'truth-tree',
                }
            ]
        });

        new TruthTree(
            document.getElementById('truth-tree'),
            isConsistent.truth_tree
        );
    }
    else if (parsedInput.get_kind() == logic_wasm.InputKind.Argument) {
        var isValid = parsedInput.is_valid();

        document.getElementById('output-tab-container').innerHTML = _computeHtmlOutput({
            message: `Argument is${isValid.is_valid ? ' ' : ' not '}valid.`,
            trees: [
                {
                    id: 'truth-tree',
                }
            ]
        });

        new TruthTree(
            document.getElementById('truth-tree'),
            isValid.truth_tree
        );
    }
    else if (parsedInput.get_kind() == logic_wasm.InputKind.Statement) {
        var isContradiction = parsedInput.is_contradiction();
        var isTautology = parsedInput.is_tautology();

        var isWhat = null;

        if (isContradiction.is_contradiction) {
            isWhat = 'contradiction';
        }
        else if (isTautology.is_tautology) {
            isWhat = 'tautology';
        }
        else {
            isWhat = 'contingency';
        }

        document.getElementById('output-tab-container').innerHTML = _computeHtmlOutput({
            message: `Statement is ${isWhat}.`,
            trees: [
                {
                    header: 'Tree for statement',
                    id: 'truth-tree-contradiction',
                },
                {
                    header: 'Tree for negation of statement',
                    id: 'truth-tree-tautology',
                }
            ]
        });

        new TruthTree(
            document.getElementById('truth-tree-contradiction'),
            isContradiction.truth_tree
        );

        new TruthTree(
            document.getElementById('truth-tree-tautology'),
            isTautology.truth_tree
        );
    }

    _moveToOutputTab();
}

function _computeHtmlParseError(e) {
    return '<div class="ui negative message">' +
        '<div class="header">' +
        'Parse Error' +
        '</div>' +
        '<pre style="overflow: auto;">' + e.message + '</pre>' +
        '</div>';
}

function _showParseError(e) {
    var errorContainer = document.getElementById('error-container');
    errorContainer.innerHTML =
        _computeHtmlParseError(e);
    errorContainer.style.marginTop = '7px';
}

function _hideAnyParseError() {
    var errorContainer = document.getElementById('error-container');
    errorContainer.innerHTML = '';
    errorContainer.style.marginTop = '0px';
}

function _computeHtmlOutput(output) {
    var outputTabHtml = '';

    outputTabHtml += '<div class="ui message">\n' +
        '<div class="header">\n' +
        output.message +
        '\n</div>\n' +
        '</div>\n' +
        '<div class="ui container grid">\n';

    for (var i = 0; i < output.trees.length; i++) {
        var tree = output.trees[i];

        outputTabHtml += '<div class="row">\n' +
            '<div class="column">\n';

        if (tree.header) {
            outputTabHtml += '<h3 class="ui header">\n' +
                `<div class="content">${tree.header}</div>` +
                '</h3>\n';
        }

        outputTabHtml += `<div class="truth-tree" id="${tree.id}" style="max-height: 800px; max-width: 100%;"></div>`;

        outputTabHtml += '\n</div>' +
            '\n</div>';
    }

    outputTabHtml += '\n</div>';

    return outputTabHtml;
}

function _moveToOutputTab() {
    $('.tabular.menu .item').removeClass('active');

    $('.tabular.menu .item[id="output-tab"]').removeClass('disabled');

    $('.tabular.menu .item[id="output-tab"]').attr('data-tab', 'output-tab');

    $.tab('change tab', 'output-tab');

    $(`.tabular.menu .item[data-tab='output-tab']`).addClass('active');
}