pure-io
=====

Pure IO monad.

Example:

``` haskell
test input =
  runIO (Input input) io
  where io :: IO Int
        io =
           do putStrLn "Enter your name!"
              name <- getLine
              putStrLn "Enter your age!"
              age <- readLn
              putStrLn ("Your name is " ++ name ++ " and your age is " ++ show age ++ "!")
              return age
```

How to run it:

``` haskell
> test []
(Left InterruptStdin
,Output {outputStdout = ["Enter your name!\n"]})
> test ["Chris"]
(Left InterruptStdin
,Output {outputStdout = ["Enter your name!\n"
                        ,"Enter your age!\n"]})
> test ["Chris","23"]
(Right 23
,Output {outputStdout = ["Enter your name!\n"
                        ,"Enter your age!\n"
                        ,"Your name is Chris and your age is 23!\n"]})
> test ["Chris","a"]
(Left (InterruptException (UserError "readIO: no parse"))
,Output {outputStdout = ["Enter your name!\n"
                        ,"Enter your age!\n"]})
```
