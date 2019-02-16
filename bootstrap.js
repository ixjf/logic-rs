// A dependency graph that contains any wasm must all be imported
// asynchronously. This `bootstrap.js` file does the single async import, so
// that no one else needs to worry about it again.

// Polyfill for MS Edge, which doesn't support TextEncoder/TextDecoder
import { TextEncoder, TextDecoder } from 'text-encoding';

window.TextEncoder = TextEncoder;
window.TextDecoder = TextDecoder;

import("./index.js")
  .catch(e => console.error("Error importing `index.js`:", e));
