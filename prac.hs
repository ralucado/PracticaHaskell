import Data.Maybe
import Data.Either
type Ident = String
data NExpr a = Var Ident
	   | Const a 
	   | Plus (NExpr a) (NExpr a) 
	   | Minus (NExpr a) (NExpr a) 
	   | Times (NExpr a) (NExpr a)
	    deriving(Read)
	     
data BExpr a = AND (BExpr a) (BExpr a)
	   | OR (BExpr a) (BExpr a)
	   | NOT (BExpr a) 
	   | Gt (NExpr a) (NExpr a) 
	   | Eq (NExpr a) (NExpr a)
	    deriving(Read)

data Command a = Assign Ident (NExpr a)
	       | Input Ident
	       | Print (NExpr a)
	       | Empty Ident
	       | Push Ident (NExpr a)
	       | Pop Ident Ident
	       | Size Ident Ident
	       | Seq [Command a]
	       | Cond (BExpr a) (Command a) (Command a)
	       | Loop (BExpr a) (Command a)
	        deriving(Read)

data SymTable a = Variable Ident a (SymTable a) (SymTable a)
				| Pila Ident [a] (SymTable a) (SymTable a)
				| TaulaBuida

class Evaluable e where
	eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
	typeCheck :: (Ident -> String) -> (e a) -> Bool

getValue :: SymTable a -> Ident -> (Maybe a)
getValue TaulaBuida k = Nothing
getValue (Variable s x left right) k 
  | k < s = getValue left k
  | k > s = getValue right k
  | otherwise = (Just x)
getValue (Pila s l left right) k
  | k < s = getValue left k
  | k > s = getValue right k
  | otherwise = (Just (head l))

getType :: SymTable a -> Ident -> String
getType TaulaBuida k = ""
getType (Variable s x left right) k
  | k < s = getType left k
  | k > s = getType right k
  | otherwise = "Variable"
getType (Pila s l left right) k
  | k < s = getType left k
  | k > s = getType right k
  | otherwise = "Pila"

instance Evaluable NExpr where
	eval f (Const x) = Right x
	eval f (Var x)
	  | f x == Nothing = Left "error"
	  | otherwise = Right (fromJust(f x))
	eval f (Plus expA expB)= case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ a + b
	eval f (Minus expA expB)= case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ a - b
	eval f (Times expA expB)= case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ a * b

instance Evaluable BExpr where
	eval f (Gt expA expB) = case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ if a > b then 1 else 0
	eval f (Eq expA expB) = case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ if a == b then 1 else 0
	eval f (OR expA expB) = case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ if a == 1 || b == 1 then 1 else 0
	eval f (AND expA expB) = case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right $ if a==1 && b==1 then 1 else 0
	eval f (NOT expr) = case (eval f expr) of
		(Left a) -> Left a
		(Right b) -> Right $ if b == 1 then 0 else 1



indenta ::  Show a => Int -> Command a -> String
indenta x (Assign s expr) = (espais x) ++ s ++ " := " ++ (show expr) ++ "\n"
indenta x (Input s) = (espais x) ++ "INPUT " ++ s ++ "\n"
indenta x (Print expr) = (espais x) ++ "PRINT " ++ (show expr) ++ "\n"
indenta x (Empty s) = (espais x) ++ "EMPTY " ++ s ++ "\n"
indenta x (Push s expr) = (espais x) ++ "PUSH " ++ s ++ " " ++ (show expr) ++ "\n"
indenta x (Pop s v) = (espais x) ++ "POP " ++ s ++ " " ++ v ++ "\n"
indenta x (Size s v) = (espais x) ++ "SIZE " ++ s ++ " " ++ v ++ "\n"
indenta x (Seq l) = (foldr (\com acc -> (indenta x com) ++ acc) "" l)
indenta x (Cond expr coma comb) = (espais x) ++ "IF " ++ (show expr) ++ " THEN\n" ++ (indenta (x+2) coma) ++ (espais x) ++ "ELSE\n" ++ (indenta (x+2) comb) ++ (espais x) ++ "END\n"
indenta x (Loop expr com) = (espais x) ++ "WHILE " ++ (show expr) ++ "\n" ++ (espais x) ++ "DO\n" ++ (indenta (x+2) com) ++ (espais x) ++ "END\n"

instance Show a => Show(NExpr a) where
    show (Var x) = x
    show (Const x) = show x
    show (Plus x y) = (show x) ++ " + " ++ (show y)
    show (Minus x y) = (show x) ++ " - " ++ (show y)
    show (Times x y) = (show x) ++ " * " ++ (show y)

instance Show a => Show(BExpr a) where
    show (AND x y) = (show x) ++ " AND " ++ (show y)
    show (OR x y) = (show x) ++ " OR " ++ (show y)
    show (NOT x) = "NOT " ++ (show x)
    show (Gt x y) = (show x) ++ " > " ++ (show y)
    show (Eq x y) = (show x) ++ " = " ++ (show y)

espais :: Int -> String
espais x
	| x <= 0 = ""
	| otherwise = " " ++ (espais (x-1))


instance Show a => Show(Command a) where
    show = indenta 0
