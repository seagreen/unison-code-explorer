run in this `client` dir with this

```bash
elm-live src/Main.elm -p 8089 -d dist -s index.html --proxyHost http://localhost:3000 --proxyPrefix='/function-call-graph' -- --output=dist/app.js
```