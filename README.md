# Deckatron

## What is it?

TL;DR see a short demo of all features https://www.youtube.com/watch?v=D3XXGWGfcCA

Deckatron is presentation platform that supports all stages of presentation lifecycle:

- Collect ideas and organize them in our WYSIWYG markdown editor
- Get auto-generated slides deck from markdown source
- Adjust theme to your taste
- Present using build-in presentation mode
- See how many people are watching your talk during presentation in real-time
- Followers can open presentaiton page and follow slides in real-time
- Followers can ask questions and vote for other people’s questions during the talk
- When you reach the end of your slides, you’ll get a screen with questions sorted by popularity
- Convert a presentation to a article/blog post by adding speaker notes/comments/text to slides
- Publish deck on Deckatron along with speaker notes using Read mode
- Fork a deck you like and change/expand/present it

Everything you see on Deckatron is real-time, synchronized using websockets under the hood.

Deckatron is single-page app built using:

- Clojure
- Http-kit
- Compojure
- ClojureScript
- Rum
- Transit (clj and cljs)
- Instaparse-cljs

Live version at http://deckatron.co

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
