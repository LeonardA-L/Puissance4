#Puissance4+

A basic implementation of a 4-In-A-Row game 

### To launch one game

```prolog
startGame.
```

### To launch several games

```prolog
startMany(10).
```

/!\ Once the AIs are chosen, there's no other to choose other than restarting puissance4.pl

------

When adding an ai, please do as follow :

* Your ai must be exported as a module with the same name as the file it is in
* In puissance4.pl, duplicate a "chooseA" line with the information (name of file/module) replaced accordingly, and a title for your AI (human, random, ...)
* Same for chooseB
* Add the name of the AI (human, random...) to the "listAI" string.