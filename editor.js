import CodeMirror from 'codemirror/lib/codemirror.js';
import 'codemirror/lib/codemirror.css';
import 'codemirror/addon/mode/simple.js';
import Cookies from 'cookies-js/dist/cookies.js';
import './editor_theme.css';
import './editor_mode.js';
import { mapObject } from './helpers.js';
import { keysMap } from './keysmap.js';
import { tokens } from './tokens.js';
import { solve } from './editor_output.js';

export function loadEditor() {
    var editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
        mode: "rpl",
        lineNumbers: true,
        theme: "custom",
        unit: "word",
        // Maps keyboard shortcuts
        extraKeys: mapObject(keysMap, function (value) {
            return function (editor) {
                _replaceOrInsertInEditor(editor, value);
            }
        }),
    });

    // Add tooltip with keyboard shortcuts for editor toolbar buttons
    document.querySelectorAll('.editor-toolbar-shortcut').forEach(e => {
        // Not all toolbar buttons have a keymap
        var keyMap = Object.entries(keysMap).find(v => {
            return v[1] == e.innerText;
        });

        if (keyMap) {
            var shortcut = keyMap[0];
            e.setAttribute("data-tooltip", shortcut);
        }
    });

    _loadStoredInputFromCookies(editor);

    _bindUiEvents(editor);
};

function _bindUiEvents(editor) {
    // Add actions for editor toolbar buttons
    document.querySelectorAll('.editor-toolbar-shortcut').forEach(e => {
        e.addEventListener('mousedown', e => {
            e.preventDefault();
        }); // Don't grab focus
        e.addEventListener('click', function () {
            _replaceOrInsertInEditor(editor, this.innerText);
        });
    });

    window.addEventListener('unload', () => {
        _saveInputToCookies(editor);
    });

    document.getElementById('solve').addEventListener('click', e => {
        solve(editor.getValue());
    });
}

function _replaceOrInsertInEditor(editor, text) {
    var doc = editor.getDoc();

    var from = doc.getCursor("from");
    var to = doc.getCursor("to");

    doc.replaceRange(text, from, to);
}

// Input is saved so one can close the window and come back
// without losing whatever was previously input (could happen
// that the window was closed accidentally while messing
// with shortcuts :))
function _saveInputToCookies(editor) {
    Cookies.set('input', editor.getValue());
}

function _loadStoredInputFromCookies(editor) {
    var savedInput = Cookies.get('input');

    if (!savedInput) {
        editor.getDoc().setValue(_defaultInput());
    }
    else {
        editor.getDoc().setValue(savedInput);
    }
}

function _defaultInput() {
    // (∀x)(H¹x ⊃ A¹x)
    // ∴ (∀x)((P¹x & (∃z)(H¹z & O²xz)) ⊃ (∃y)(A¹y & O²xy))
    return [
        tokens.grouperOpening, tokens.universalQuantifier, 'x', tokens.grouperClosing,
        tokens.grouperOpening, 'H', tokens.superscriptNumber1, 'x', ' ', tokens.conditional,
        ' ', 'A', tokens.superscriptNumber1, 'x', tokens.grouperClosing, '\n',

        tokens.conclusionIndicator, ' ', tokens.grouperOpening, tokens.universalQuantifier,
        'x', tokens.grouperClosing, tokens.grouperOpening, tokens.grouperOpening,
        'P', tokens.superscriptNumber1, 'x', ' ', tokens.conjunction, ' ', tokens.grouperOpening,
        tokens.existentialQuantifier, 'z', tokens.grouperClosing, tokens.grouperOpening,
        'H', tokens.superscriptNumber1, 'z', ' ', tokens.conjunction, ' ', 'O',
        tokens.superscriptNumber2, 'x', 'z', tokens.grouperClosing, tokens.grouperClosing,
        ' ', tokens.conditional, ' ', tokens.grouperOpening, tokens.existentialQuantifier, 'y',
        tokens.grouperClosing, tokens.grouperOpening, 'A', tokens.superscriptNumber1, 'y',
        ' ', tokens.conjunction, ' ', 'O', tokens.superscriptNumber2, 'x', 'y',
        tokens.grouperClosing, tokens.grouperClosing,
    ].join('');
}