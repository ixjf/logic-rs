# wasm-layer

Layer for communication between the website and logic-rs

## Dependencies
 - npm
 - wasm-pack (or just wasm-bindgen if you like to type)
 - wasm-bindgen-cli (`cargo install wasm-bindgen-cli`)

## Building
Run `wasm-pack build` in ./

## Linking to wasm-layer from the website
 1) `npm link` in ./pkg/ after building wasm-layer
 2) `npm link wasm-layer` in the website source's directory