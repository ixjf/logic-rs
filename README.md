# www

Website source code

## Dependencies
 - npm

## Developing
Run `npm install`, making sure webpack, webpack-cli, and webpack-dev-server are installed.

Run `npm run start` in ./ and browse to http://localhost:8080

To modify Semantic UI, make your changes in ./semantic/src/site/ and then run `gulp build` in ./semantic.
Alternatively, run `gulp watch` once (also in ./semantic) and leave it running in the background.

## Packaging
Run `npx webpack` in ./. Output will be in ./dist/