all: clientside/index.html serverside/server generated/Main.js

serverside/server:
	cd serverside && cabal  --require-sandbox exec ghc  -- Main.hs --make -o server && cd ..

generated/index.html:
	jade --obj clientside/index.json clientside/index.jade

generated/Main.js: 
	cd clientside && pulp browserify --to ../generated/Main.js && cd ..

runserver: all
	cd serverside && server