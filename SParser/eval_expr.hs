import SParser
import qualified Data.Map as Map
import System.IO
import Control.Monad
import Control.Applicative
import Char
-----------------------------------------------------------------
type Val        = Int
data Expr       = Num Val | Var String | BinOp String Expr Expr | Assign Expr Expr
                deriving Show
--
var, numval, factor, term :: Parser Expr

var             = token $ do
                    l       <- alpha
                    rest    <- many alphanum
                    return $ Var (l:rest)

numval          = Num <$> number

paren           = "(" -# expr #- ")"

factor          = var <|> numval <|> paren

binop :: String -> Parser (Expr -> Expr -> Expr)
binop s         = BinOp <$> accept s

mulOp           = binop "*" <|> binop "/"
addOp           = binop "+" <|> binop "-"

term            = factor <**> mulOp <*> term
                  <|> factor
--
assignOp        = "=" -# return Assign
assignment      = var <**> assignOp <*> expr

expr            = assignment
                  <|> term <**> addOp <*> expr
                  <|> term
--
-----------------------------------------------------------------

type VarMap = Map.Map String Val

eval' :: VarMap -> Expr -> (Maybe Val, VarMap)
eval' vm (Var v)    = (Map.lookup v vm, vm)
eval' vm (Num n)    = (Just n, vm)
eval' vm (Assign (Var v) e)
                    = case (eval' vm e) of
                        (Just n, vm') -> (Just n, Map.insert v n vm')
                        _ -> (Nothing, vm)
eval' vm (BinOp op l r)
                    = case eval' vm r of
                          (Just r', vm') -> 
                              case eval' vm' l of
                                  (Just l', vm'') -> (Just (op' l' r'), vm'')
                                  _ -> (Nothing, vm)
                          _ -> (Nothing, vm)
                    where op' = case op of
                            "+" -> (+)
                            "-" -> (-)
                            "*" -> (*)
                            "/" -> (div)

eval :: VarMap -> String -> (Maybe Val, VarMap)
eval vm cs          = case parse expr cs of
                          ([], Just e) -> eval' vm e
                          _ -> (Nothing, vm)

-----------------------------------------------------------------

main =
    do  let vm = Map.fromList [] :: VarMap
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        mainLoop vm

mainLoop vm =
    do  putStr "> "
        inp <- getLine
        case inp of
            "quit" -> return ()
            "list" ->
                do  listVars vm
                    mainLoop vm
            "" -> mainLoop vm
            _ ->
                do  let (res, vm') = eval vm inp
                    case res of
                        Just n -> putStrLn $ show n
                        _ -> putStrLn $ "error: " ++ inp
                    mainLoop vm'

listVars vm =
    do  putStrLn "------------------"
        putStrLn "Defined Variables:"
        forM (Map.toList vm) (\(var, val) -> 
                putStrLn $ (var ++ "=" ++ show val))
        putStrLn "------------------"
