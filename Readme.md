# Einmaleins-Trainer in elm

Small elm-app to learn [elm](https://elm-lang.org/) and build a side, 
where you can train the multiplication table, 
in German "Einmaleins". 

## How to run
- Development with reactor doesn't work anymore see 
[stackoverflow for oldapproaches](https://stackoverflow.com/questions/41333765/using-elm-reactor-with-elm-embedded-in-html)

- Development call
`elm make --debug src/Main.elm --output=public/js/main.js`
or
```shell script
 while inotifywait -e close_write src/Main.elm; do elm make --debug src/Main.elm --output=public/js/main.js; done
```

- Production call
`elm make --optimize src/Main.elm --output=public/js/main.js`
Copy everything from public into your preferred webspace.

Open the index.html in your browser. If you want to work with reactor you must use