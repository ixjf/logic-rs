import { TextEncoder, TextDecoder } from 'text-encoding';
import './semantic/dist/semantic.min.css';
import './semantic/dist/semantic.min.js';
import './css/index.css';
import { loadEditor } from './editor.js';
import { mapHtmlTokens } from './tokens_html_map.js';

// Polyfill for Edge, which doesn't support TextEncoder and TextDecoder
window.TextDecoder = TextDecoder;
window.TextEncoder = TextEncoder;


window.addEventListener('load', () => {
    mapHtmlTokens(); // This must _always_ load before anything else!
    loadUiComponents();
});

function loadUiComponents() {
    loadEditor();

    $('.ui.accordion').accordion();
    $('.tabular.menu .item').tab();
    $('.ui.longer.modal').modal();

    bindMiscUiEvents();
}

function bindMiscUiEvents() {
    document.getElementById('show-grammar-modal').addEventListener('click', e => {
        $('#grammar-modal').modal('show');
    });
}