pure-io
=====

Pure IO monad.

Example:

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
