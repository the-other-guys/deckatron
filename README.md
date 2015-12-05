# Deckatron

## Dev setup

```sh
lein figwheel &
open http://localhost:8080/
```

This will watch, recompile and auto-reload CLJS app and CSS files.

## Prod setup

```sh
lein package
java -jar target/deckatron.jar &
open http://localhost:8080/
```

Here `lein package` is just an alias for `lein do cljsbuild once advanced, uberjar`.
