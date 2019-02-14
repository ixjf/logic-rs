import { loadEditor } from './editor.js';
import { mapHtmlTokens } from './tokens_html_map.js';

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