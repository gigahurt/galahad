To develop:
* Install node
* npm install -g elm
* elm-reactor
* Navigate to the url printed out by the elm-reactor command
* Changes reflect instantly after saving file, just refresh browser

Todo list:
* Finish client side game logic
* Create server side code to allow web socket connections, for now:
  * Assume there will only be 1 server, no need for SNS etc
  * Store completed games in dynmo or rds
  * Store active games in memory
  * Be able to use http to query active and completed games
  * Each active game can be listened to through web sockets
* Write code on client side to publish moves to web socket 
* Write a view to 'watch' a game
* Deploy scripts for elastic bean stalk

Communicate at gitter.im/galahad.is-this-normal.net/