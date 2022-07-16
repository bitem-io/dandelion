# Prerequisite
- clojure cli
- nodejs, npm, yarn

# Server Side

## to run
$ lein run

## to test
$ lein test

# Client Side

## Structure
- Reference: https://www.youtube.com/watch?v=p61lhOvQg2Q&t=337s

## Run on terminal
$ yarn install
$ yarn start

## Connect REPL
m-x cider-connect
select 'localhost'
enter port 9000
shadow.user> (shadow/repl :app)

## Deployment
Netlify.
- Connect the git repository in Netlify.
- Build command: `yarn release`
- Publish director: `resources/public/`
- Reference: https://www.emcken.dk/programming/2022/02/20/shadow-cljs-and-netlify/
