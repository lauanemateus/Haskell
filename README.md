**Some basic informations of Haskell**

* You can install the interactive Haskell compiler on Ubuntu using the following command:

```sudo apt install ghc```

* If the installation is successful, running the command `ghci` should start the interactive compiler in the terminal.
* To save a Haskell code file, use the extension `.hs`, for example, `code_name.hs`.
* After starting the interactive compiler with `ghci`, you can load a Haskell code file by typing `:load code_name.hs`. Then, you can call a function with its parameters.

Exemple:

if your "code_name.hs" is

```
sum x y = x+y
```

You can do:
```
:load code_name.hs
sum 2 3
```

and it has to show ```5``` on terminal.

* To clear the terminal:
  
  `:! clear`
