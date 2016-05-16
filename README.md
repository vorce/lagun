# Lagun

[![Heroku](http://heroku-badge.herokuapp.com/?app=trylagun&style=flat)](http://trylagun.herokuapp.com/)

An interactive web UI for Open API (2.0) specs. My first toy project in Elm.

Uses:

- [Milligram](https://milligram.github.io/)
- [Font Awesome](https://fortawesome.github.io/Font-Awesome/)
- [heroku-buildpack-elm](https://github.com/srid/heroku-buildpack-elm) (Waiting for [Update for Elm 0.17](https://github.com/srid/heroku-buildpack-elm/issues/16) to be able to deploy latest version of Lagun to Heroku)

[Demo on Heroku](http://trylagun.herokuapp.com/)

## Build

    elm-make Main.elm --warn --output elm.js

Open `index.html`

## Supported specification formats

- [Open API 2.0](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md)

I doubt I will add support for other specs (such as RAML). PRs welcome ;)

## TODO / Explore

- Add support for remaining parameter types: body, query
- Remove hardcoded host and basePath for requests :-1:
- Show schema types for body parameters (ugh)
- Show response model for http responses
- Show headers for http responses
- Some tests would be fun and useful
- Show version of Lagun somewhere on the page (bottom?)
