# Lagun

[![Heroku](http://heroku-badge.herokuapp.com/?app=trylagun&style=flat)](https://trylagun.herokuapp.com/)

An interactive web UI for Open API (2.0) specs. My first (toy) project in Elm!

Uses [Milligram](https://milligram.github.io/).

[Demo on Heroku](https://trylagun.herokuapp.com/) - Note that you may have to disable protection in your browser (I will look into this, but yeah..)

## Build

    elm-make Main.elm --output elm.js

Open `index.html`

## Supported specification formats

- [Open API 2.0](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md)

I doubt I will add support for other specs (such as RAML). PRs welcome ;)

## TODO / Explore

- Add possibility to try out requests. Buttons! Not very interactive without this. :zap:
- Complete json parsing into model. Move that code into its own file.
- Some tests would be fun and useful
