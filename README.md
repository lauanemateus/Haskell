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

------------------------------------------------------------------------
* To see the all functions available:

  `:browse`
* To import a module use : `:load Module_name`
  
  `:load Data.list`
* To see all the functions of a especific module: `:browse Module_name`
  
  `:browse Data.List`
* To see the function signature: `:info function_name`
  
  `:info map`
* To see more detailed information: `:doc function_name`
  
  `:doc map`
