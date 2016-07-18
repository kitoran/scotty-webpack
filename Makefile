all: generated/index.html serverside/server generated/Main.js

serverside/server: serverside/Main.hs
	cd serverside && cabal  --require-sandbox exec ghc  -- Main.hs --make -o server && cd ..

generated/index.html: clientside/index.json clientside/index.jade
	jade --obj clientside/index.json clientside/index.jade --out generated

generated/Main.js: clientside/src/Main.purs
	cd clientside && pulp browserify --to ../generated/Main.js && cd ..

runserver: generated/index.html serverside/server generated/Main.js
	serverside/server
