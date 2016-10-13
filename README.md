# Lagun

[![Heroku](http://heroku-badge.herokuapp.com/?app=trylagun&style=flat)](http://trylagun.herokuapp.com/)

An interactive web UI for Open API (2.0) specs. Think of it as an alternative to the default [Swagger UI](https://github.com/swagger-api/swagger-ui).
My first project for learning and exploring Elm.

Uses:

- [Milligram](https://milligram.github.io/)
- [Font Awesome](https://fortawesome.github.io/Font-Awesome/)
- [heroku-buildpack-elm](https://github.com/srid/heroku-buildpack-elm)

[Demo on Github pages for swagger's petstore sample API](http://vorce.github.io/lagun/) (also available on [trylagun.herokuapp.com](http://trylagun.herokuapp.com/))

## Build

    elm-make src/Main.elm --warn --output elm.js

or

    make

If everything compiles fine open `index.html` to use Lagun.
If you want to change the default specification url simply edit `index.html`, and modify the `specUrl` setting passed into `Elm.Main.fullscreen`.
This could also be a path to a file (for example '/docs/myspec.json'). You can also set the specUrl by providing a `spec` query parameter to index.html.
For example: `http://lagunlocation.com/index.html?spec=http://speclocation/swagger`

## Supported specification formats

- [Open API 2.0](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md)

I doubt I will add support for other specs (such as [RAML](http://raml.org/)). PRs welcome ;)

## TODO / Explore

- Add support for remaining parameter types: formData
- Show schema types for body parameters (ugh)
- Show response model for http responses
- Some tests would be fun and useful
- Show version of Lagun somewhere on the page (bottom?)
- Structure the code in a nicer way

