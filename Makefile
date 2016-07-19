
all: build ;

build: elm.js ;

clean:
	rm elm.js

elm.js: Main.elm Lagun.elm View.elm elm-package.json
	elm-make Main.elm --warn --output elm.js
