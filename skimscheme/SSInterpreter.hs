{-

A basic interpreter for a purely functional subset of Scheme named SkimScheme.
Part of this interpreter has been derived from the "Write Yourself a Scheme in
48 Hours - An Introduction to Haskell through Example", by Jonathan Tang. It
does not implement a number of Scheme's constructs. Moreover, it uses a
different approach to implement mutable state within the language.

The name "SkimScheme" refers to the stripped down nature of this interpreter.
According to the New Oxford American Dictionary, "skim" can mean:

(as a verb) ... read (something) quickly or cursorily so as to note only
the important points.

(as a noun) ... an act of reading something quickly or superficially. 

"skimmed/skim milk" is milk from which the cream has been removed. 

The name emphasizes that we do not want to cover the entire standard, small as
it may be. Instead, we want to focus on some of the important aspects, taking a
language implementer's point of view, with the goal of using it as a teaching
tool. Many, many, many aspects of Scheme standards are not covered (it does not
even support recursion!).

Written by Fernando Castor
Started at: August 28th 2012
Last update: December 17th 2012

-}

module Main where
import Control.Applicative hiding (empty)
import System.Environment
import Control.Monad
import Data.Map as Map
import LispVal
import SSParser
import SSPrettyPrinter

-----------------------------------------------------------
--                      INTERPRETER                      --
-----------------------------------------------------------
eval :: StateT -> LispVal -> StateTransformer LispVal
eval env val@(String _) = return val
eval env val@(Atom var) = stateLookup env var 
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List (Atom "if": exp: thn: els:[])) = (eval env exp) >>= (\v -> case v of { (error@(Error _)) -> return error; (boolean@(Bool b)) -> (if (b == True) then eval env thn else eval env els) ; otherwise -> return (Error "etreer")})
eval env (List (Atom "if": exp: thn:[])) = (eval env exp) >>= (\v -> case v of { (error@(Error _)) -> return error; (boolean@(Bool b)) -> (if (b == True) then eval env thn else return $ List []) ; otherwise -> return (Error "etreer")})
{--
runhaskell SSInterpreter.hs "(if #t 'yes 'no)
yes
[]
runhaskell SSInterpreter.hs "(if (#f) 'yes 'no)"
no
[]
runhaskell SSInterpreter.hs "(if (boolean? #t) 'yes 'no)"
yes
[]
runhaskell SSInterpreter.hs "(if (boolean? #f) 'yes 'no)"
yes
[]
runhaskell SSInterpreter.hs "(if #t 2 3)"
2
[]
runhaskell SSInterpreter.hs "(if #f 2 3)"
3
[]
--}
eval env (List (Atom "set!": (Atom var): exp: [])) = stateLookup env var >>= (\v -> case v  of { (error@(Error _)) -> return error; otherwise -> defineVar env var exp})
{--
runhaskell SSInterpreter.hs "(begin (define x 2) (set! x 4))"
4
[("x",4)]

runhaskell SSInterpreter.hs "(set! x 4)"
variable does not exist.
[]
--}
eval env (List (Atom "comment": _)) = return Comment
{-
runhaskell SSInterpreter.hs "(comment Projeto de PLC)"
[]
runhaskell SSInterpreter.hs "(comment)"
[]
runhaskell SSInterpreter.hs "(comment 1 2 3 4 \"isto eh um comentario\")"
[]
-}
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "begin":[v])) = eval env v
eval env (List (Atom "begin": l: ls)) = (eval env l) >>= (\v -> case v of { (error@(Error _)) -> return error; otherwise -> eval env (List (Atom "begin": ls))})
eval env (List (Atom "begin":[])) = return (List [])
eval env lam@(List (Atom "lambda":(List formals):body:[])) = return lam
eval env lam@(List (List (Atom "lambda":(List formals):body:[]):args)) = mapM (eval env) args >>= (lambda env formals body)
eval env (List (Atom "let":bindings:body:[])) = (flet env (separa bindings) body)
-- runhaskell SSInterpreter.hs "(begin (define x 10) (let ((x 5) (y (* x 2))) (+ x y)))""

eval env (List (Atom "make-closure":lambda:[])) = return (Closure lambda env)
-- runhaskell SSInterpreter.hs "(begin (let ((i 1)) (define f (make-closure (lambda (y) (begin (set! i (+ i y))) )) )) (define val1 (f 1)) (define val2 (f 2)) (+ val1 val2) )"

-- The following line is slightly more complex because we are addressing the
-- case where define is redefined by the user (whatever is the user's reason
-- for doing so. The problem is that redefining define does not have
-- the same semantics as redefining other functions, since define is not
-- stored as a regular function because of its return type.
eval env (List (Atom "define": args)) = maybe (define env args) (\v -> return v) (Map.lookup "define" env)
eval env (List (Atom func : args)) = mapM (eval env) args >>= apply env func 
eval env (Error s)  = return (Error s)
eval env form = return (Error ("Could not eval the special form: " ++ (show form)))

separa :: LispVal -> ([LispVal], [LispVal])
separa (List []) = ([], [])
separa (List ((List (id:val:[])):ls)) = ((id:ids), (val:vals))
  where (ids, vals) = separa (List ls)

flet ::  StateT -> ([LispVal], [LispVal]) -> LispVal -> StateTransformer LispVal
flet env (formals, args) body =  mapM (eval env) args >>= (lambda env formals body)

stateLookup :: StateT -> String -> StateTransformer LispVal
stateLookup env var = ST $ 
  (\s -> 
    (maybe (Error "variable does not exist.") 
           id (Map.lookup var (union env s)  -- recursão
    ), s))


-- Because of monad complications, define is a separate function that is not
-- included in the state of the program. This saves  us from having to make
-- every predefined function return a StateTransformer, which would also
-- complicate state management. The same principle applies to set!. We are still
-- not talking about local definitions. That's a completely different
-- beast.
define :: StateT -> [LispVal] -> StateTransformer LispVal
define env [(Atom id), val] = defineVar env id val
define env [(List [Atom id]), val] = defineVar env id val
-- define env [(List l), val]                                       
define env args = return (Error "wrong number of arguments")
defineVar env id val = 
  ST (\s -> let (ST f)    = eval env val
                (result, newState) = f s
            in (result, (insert id result newState))
     )


-- The maybe function yields a value of type b if the evaluation of 
-- its third argument yields Nothing. In case it yields Just x, maybe
-- applies its second argument f to x and yields (f x) as its result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply :: StateT -> String -> [LispVal] -> StateTransformer LispVal
apply env func args =  
                  case (Map.lookup func env) of
                      Just (Native f)  -> return (f args)
                      otherwise -> 
                        (stateLookup env func >>= \res -> 
                          case res of 
                            (Closure (List (Atom "lambda" : List formals : body:l)) env2) -> lambda env2 formals body args                        
                            List (Atom "lambda" : List formals : body:l) -> lambda env formals body args       
                            otherwise -> return (Error $ func ++ " not a function.")
                        )
-- The lambda function is an auxiliary function responsible for
-- applying user-defined functions, instead of native ones. We use a very stupid 
-- kind of dynamic variable (parameter) scoping that does not even support
-- recursion. This has to be fixed in the project.
lambda :: StateT -> [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
lambda env formals body args = 
  let dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) env (zip formals args)
  in  eval dynEnv body


-- Initial environment of the programs. Maps identifiers to values. 
-- Initially, maps function names to function values, but there's 
-- nothing stopping it from storing general values (e.g., well-known
-- constants, such as pi). The initial environment includes all the 
-- functions that are available for programmers.
environment :: Map String LispVal
environment =   
            insert "number?"        (Native predNumber)
          $ insert "boolean?"       (Native predBoolean)
          $ insert "list?"          (Native predList)
          $ insert "+"              (Native numericSum) 
          $ insert "*"              (Native numericMult) 
          $ insert "cons"           (Native cons) -- analogo ao : de haskell 
          $ insert "lt?"            (Native boolLt) -- less than 
          $ insert "/"              (Native numericDiv) -- divisao inteira entre numeros 
          $ insert "mod"            (Native numericMod) -- resto da divisao inteira
          $ insert "eqv?"           (Native compareValue) -- comparacao entre valores 
          $ insert "-"              (Native numericSub) 
          $ insert "car"            (Native car)           
          $ insert "cdr"            (Native cdr)           
            empty

type StateT = Map String LispVal

-- StateTransformer is a data type that embodies computations
-- that transform the state of the interpreter (add new (String, LispVal)
-- pairs to the state variable). The ST constructor receives a function
-- because a StateTransformer gets the previous state of the interpreter 
-- and, based on that state, performs a computation that might yield a modified
-- state (a modification of the previous one). 
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
  return x = ST (\s -> (x, s))
  (>>=) (ST m) f = ST (\s -> let (v, newS) = m s
                                 (ST resF) = f v
                             in  resF newS
                      )

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

-----------------------------------------------------------
--          HARDWIRED PREDEFINED LISP FUNCTIONS          --
-----------------------------------------------------------

-- Includes some auxiliary functions. Does not include functions that modify
-- state. These functions, such as define and set!, must run within the
-- StateTransformer monad. 

car :: [LispVal] -> LispVal
car [List (a:as)] = a
car [DottedList (a:as) _] = a
car ls = Error "invalid list."

cdr :: [LispVal] -> LispVal
cdr (List (a:as) : ls) = List as
cdr (DottedList (a:[]) c : ls) = c
cdr (DottedList (a:as) c : ls) = DottedList as c
cdr ls = Error "invalid list."

predNumber :: [LispVal] -> LispVal
predNumber (Number _ : []) = Bool True
predNumber (a:[]) = Bool False
predNumber ls = Error "wrong number of arguments."

predBoolean :: [LispVal] -> LispVal
predBoolean (Bool _ : []) = Bool True
predBoolean (a:[]) = Bool False
predBoolean ls = Error "wrong number of arguments."

predList :: [LispVal] -> LispVal
predList (List _ : []) = Bool True
predList (a:[]) = Bool False
predList ls = Error "wrong number of arguments."

numericSum :: [LispVal] -> LispVal
numericSum [] = Number 0
numericSum l = numericBinOp (+) l

numericMult :: [LispVal] -> LispVal
numericMult [] = Number 1
numericMult l = numericBinOp (*) l

cons :: [LispVal] -> LispVal -- analogo ao : do haskell
cons [a,(List l)] = List (a:l)
cons [a,(DottedList l b)] = DottedList (a : l) b
cons _ = Error "wrong arguments."
{--
runhaskell SSInterpreter.hs "(define x (cons 2 '(3 4 6 8 10)))"
(2 3 4 6 8 10)
[("x",(2 3 4 6 8 10))]

Caso seja uma DottedList:

runhaskell SSInterpreter.hs "(define x (cons 2 '(3 . (4 . (6 . (8 . 10))))))"
(2 3 . (4 . (6 . (8 . 10))))
[("x",(2 3 . (4 . (6 . (8 . 10)))))]
--}

boolLt :: [LispVal] -> LispVal -- less than
boolLt [(Number num1), (Number num2)]
    | num1 >= num2 = Bool False
    | otherwise = Bool True
boolLt _ = Error "wrong arguments."
{--
runhaskell SSInterpreter.hs "(define x (lt? 20 2))"
#f
[("x",#f)]

runhaskell SSInterpreter.hs "(define x (lt? 2 20))"
#t
[("x",#t)]
--}

numericDiv :: [LispVal] -> LispVal -- divisao inteira
numericDiv [] = Number 0
numericDiv l = numericBinOp (div) l
{--
runhaskell SSInterpreter.hs "(define x (/ 20 2 5))"
2
[("x",2)]
--}

numericMod :: [LispVal] -> LispVal -- resto da divisao inteira
numericMod [] = Number 0
numericMod l = numericBinOp (mod) l
{--
runhaskell SSInterpreter.hs "(define x (mod 9 4))"
1
[("x",1)]
--}

compareValue :: [LispVal] -> LispVal
compareValue [Bool a, Bool b] = (Bool (a == b))
compareValue [Number a, Number b] = (Bool (a == b))
compareValue [String a, String b] = (Bool (a == b))
compareValue [List a, List b] = (Bool (eqList a b))
compareValue [DottedList a b, DottedList c d] = (Bool (eqDotted a b c d))

compareValue [_, _] = (Bool False) -- tipos diferentes

{-
runhaskell SSInterpreter.hs "(define x (eqv? (* 2 2) (/ 16 4)))"
runhaskell SSInterpreter.hs "(define x (eqv? #t #f))"
runhaskell SSInterpreter.hs "(define x (eqv? \"plc\" \"plc\"))"
runhaskell SSInterpreter.hs "(define x (eqv? (cons 2 '(3 4 5)) (cons 2 '(3 4 5))))"
runhaskell SSInterpreter.hs "(define x (eqv? '(1 . 2) '(1 . 3)))"
runhaskell SSInterpreter.hs "(define x (eqv? 1 #t))"
-}

eqList :: [LispVal] -> [LispVal] -> Bool
eqList [] [] = True
eqList [] _ = False
eqList _ [] = False
eqList (a:as) (b:bs)
 | resp = eqList as bs
 | otherwise = False
 where Bool resp = (compareValue [a, b])

eqDotted :: [LispVal] -> LispVal -> [LispVal] -> LispVal -> Bool
eqDotted a b c d
 | (eqList a c) && resp = True
 | otherwise = False
 where Bool resp = (compareValue [b, d])

numericSub :: [LispVal] -> LispVal
numericSub [] = Error "wrong number of arguments."
-- The following case handles negative number literals.
numericSub [x] = if onlyNumbers [x]
                 then (\num -> (Number (- num))) (unpackNum x)
                 else Error "not a number."
numericSub l = numericBinOp (-) l

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = if onlyNumbers args 
                       then Number $ foldl1 op $ Prelude.map unpackNum args 
                       else Error "not a number."
                       
onlyNumbers :: [LispVal] -> Bool
onlyNumbers [] = True
onlyNumbers (Number n:ns) = onlyNumbers ns
onlyNumbers ns = False             
                       
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--- unpackNum a = ... -- Should never happen!!!!

-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showResult :: (LispVal, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer LispVal -> (LispVal, StateT)
getResult (ST f) = f empty -- we start with an empty state. 

main :: IO ()
main = do args <- getArgs
          putStr $ showResult $ getResult $ eval environment $ readExpr $ concat $ args 
          
