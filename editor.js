import CodeMirror from 'codemirror/lib/codemirror.js';
import 'codemirror/lib/codemirror.css';
import 'codemirror/theme/duotone-dark.css';
import 'codemirror/addon/mode/simple.js';
import './editor.mode';
import './css/editor.mode.css';
import { mapObject } from './obj_utils';
import { keysMap } from './keysmap';

export var inputEditor = CodeMirror.fromTextArea(document.getElementById("input-textarea"), {
    mode: "rpl",
    lineNumbers: true,
    theme: "duotone-dark",
    unit: "word",
    extraKeys: mapObject(keysMap, function (value) {
        return function (editor) {
            editor.replaceSelection(value);
        }
    })
});

document.querySelectorAll('.editor-button-group-btn').forEach(function (element) {
    element.addEventListener('mousedown',
        e => e.preventDefault());
    element.addEventListener('click', function () {
        inputEditor.replaceSelection(this.innerText)
    });
});