
all: build ;

build: elm.js ;

clean:
	rm elm.js

elm.js:
	elm-make Main.elm --warn --output elm.js
