
all: build ;

build: elm.js ;

clean:
	rm elm.js

elm.js: src/Main.elm src/Lagun.elm src/View.elm elm-package.json
	elm-make src/Main.elm --warn --output elm.js
