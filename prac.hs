import Data.Maybe
import Data.Either
import System.IO
import System.Random

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
				| TaulaBuida deriving (Show)

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
  | null l = Nothing
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
	  | f x == Nothing = Left "Error: undefined variable"
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
	
	typeCheck f (Const x) = False
	typeCheck f (Var x) = ("Variable" == f x)
	typeCheck f (Plus expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (Minus expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (Times expA expB) = typeCheck f expA && typeCheck f expB

instance Evaluable BExpr where
	eval f (Gt expA expB) = case (eval f expA, eval f expB) of
		(Left a, Left b) -> Left $ a ++ ", " ++ b
		(_, Left b) -> Left b
		(Left a, _) -> Left a
		(Right a, Right b) -> Right (if a > b then 1 else 0)
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

	typeCheck f (Gt expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (Eq expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (OR expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (AND expA expB) = typeCheck f expA && typeCheck f expB
	typeCheck f (NOT expr) = typeCheck f expr

putVar :: (Num a, Ord a) => SymTable a -> Ident -> a -> SymTable a
putVar TaulaBuida s x = Variable s x TaulaBuida TaulaBuida
putVar (Variable ident valor l r) s x
  | s < ident = (Variable ident valor (putVar l s x) r)
  | s > ident = (Variable ident valor l (putVar r s x))
  | otherwise = (Variable ident x l r)
putVar (Pila ident valor l r) s x
  | s < ident = (Pila ident valor (putVar l s x) r)
  | s > ident = (Pila ident valor l (putVar r s x))
  | otherwise = (Variable ident x l r)

putStack :: (Num a, Ord a) => SymTable a -> Ident -> [a] -> SymTable a
putStack TaulaBuida s x = Pila s x TaulaBuida TaulaBuida
putStack (Variable ident valor l r) s x
  | s < ident = (Variable ident valor (putStack l s x) r)
  | s > ident = (Variable ident valor l (putStack r s x))
  | otherwise = (Pila ident x l r)
putStack (Pila ident valor l r) s x
  | s < ident = (Pila ident valor (putStack l s x) r)
  | s > ident = (Pila ident valor l (putStack r s x))
  | otherwise = (Pila ident x l r)

getStack :: SymTable a -> Ident -> (Maybe [a])
getStack TaulaBuida k = Nothing
getStack (Variable s x left right) k
  | k < s = getStack left k
  | k > s = getStack right k
  | otherwise = Nothing
getStack (Pila s l left right) k
  | k < s = getStack left k
  | k > s = getStack right k
  | otherwise = Just l

asign :: (Num a, Ord a) => SymTable a -> Ident -> NExpr a -> [a] -> ((Either String [a]),SymTable a, [a])
asign t s expr input = case eval (getValue t) expr of 
							Left err -> (Left err, t, input)
							Right x -> (Right [], putVar t s x, input)

throwErr :: (Num a, Ord a) => String ->  SymTable a -> [a] -> ((Either String [a]),SymTable a, [a])
throwErr err t i = ((Left err),t,i)

printExpr :: (Num a, Ord a) => SymTable a -> NExpr a -> [a] ->  ((Either String [a]),SymTable a, [a])
printExpr t expr input 
  | typeCheck (getType t) expr = case eval (getValue t) expr of 
									Left err -> (Left err, t, input)
									Right x -> (Right (x:[]), t, input)
  | otherwise = (Left ("Error in PRINT: type error"), t, input)

empty :: (Num a, Ord a) => SymTable a -> Ident -> [a] -> ((Either String [a]),SymTable a, [a])
empty t s input = (Right [], putStack t s [], input)

push :: (Num a, Ord a) => SymTable a -> Ident -> NExpr a -> [a] -> ((Either String [a]),SymTable a, [a])
push t s expr input = case eval (getValue t) expr of 
													 Left err -> (Left err, t, input)
													 Right x -> sortida where sortida = case getStack t s of
													 										Just cosa -> (Right [], putStack t s (x:cosa), input)
													 				 						Nothing -> (Left ("Error in PUSH: undefined variable "++s), t, input)

pop :: (Num a, Ord a) => SymTable a -> Ident -> Ident -> [a] -> ((Either String [a]),SymTable a, [a])
pop t s dest input
	| getType t s == "Pila" && getValue t s == Nothing = (Left ("Error in POP: empty stack "++s), t, input)
	| getType t s == "Pila" 						   = (Right [], putStack (putVar t dest cosa) s (tail cosa2), input)
	| getType t s == "" = (Left ("Error in POP: undefined variable "++s), t, input)
	| otherwise = (Left ("Error in POP: type error with "++s), t, input)
	where Just cosa = getValue t s
	      Just cosa2 = getStack t s

size :: (Num a, Ord a) => SymTable a -> Ident -> Ident -> [a] -> ((Either String [a]),SymTable a, [a])
size t s d input
  | getType t s == "Pila" = case getStack t s of 
  												Just x -> (Right [], putVar t d (fromIntegral(length x)), input)
  												Nothing -> (Left ("Error in SIZE: undefined variable "++s), t, input)
  | getType t s == "" = (Left ("BIS Error in SIZE: undefined variable "++s), t, input)
  | otherwise = (Left ("Error in SIZE: type error with "++s), t, input)


interpretAccumulator :: (Num a, Ord a) =>  ((Either String [a]),SymTable a, [a]) -> Command a -> ((Either String [a]),SymTable a, [a])
interpretAccumulator (Left err, t, i) _ = (Left err, t, i)
interpretAccumulator acc com = case interpretCommand (table acc) (input acc) com of 
																					(Left err, _, _) -> (Left err, table acc, input acc)
																					(Right l, t, i ) -> (Right (outacc++l), t, i)
																					where Right outacc = output acc


interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]),SymTable a, [a])
interpretCommand t i (Assign s expr) = asign t s expr i
interpretCommand t i (Input s) = asign t s (Const (head i)) (tail i)
interpretCommand t i (Print expr) = printExpr t expr i
interpretCommand t i (Empty s) = empty t s i
interpretCommand t i (Push s expr) = push t s expr i
interpretCommand t i (Pop s var) = pop t s var i
interpretCommand t i (Size s var) = size t s var i
interpretCommand t i (Seq l) = foldl (interpretAccumulator) ((Right []),t,i) l
interpretCommand t i (Cond expr thencom elsecom) = case eval (getValue t) expr of
													Right 1 -> interpretAccumulator (Right [],t,i) thencom
													Right 0 -> interpretAccumulator (Right [],t,i) elsecom
													Left x -> (Left x, t, i)
interpretCommand t i (Loop expr com) = case (eval (getValue t) expr) of
													Right 1 -> interpretAccumulator ((Right outacc),(table cosa),(input cosa)) (Loop expr com)
													Right 0 -> (Right [], t, i)
													Left x -> (Left x, t, i)
													where cosa = interpretAccumulator (Right [],t,i) com
													      Right outacc = output cosa

interpretProgram:: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input com = output (interpretCommand TaulaBuida input com)

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

instance Show a => Show(Command a) where
    show = indenta 0

instance Show a => Show(BExpr a) where
    show (AND x y) = (show x) ++ " AND " ++ (show y)
    show (OR x y) = (show x) ++ " OR " ++ (show y)
    show (NOT x) = "NOT " ++ (show x)
    show (Gt x y) = (show x) ++ " > " ++ (show y)
    show (Eq x y) = (show x) ++ " = " ++ (show y)

instance Show a => Show(NExpr a) where
    show (Var x) = x
    show (Const x) = show x
    show (Plus x y) = (show x) ++ " + " ++ (show y)
    show (Minus x y) = (show x) ++ " - " ++ (show y)
    show (Times x y) = (show x) ++ " * " ++ (show y)

espais :: Int -> String
espais x
  | x <= 0 = ""
  | otherwise = " " ++ (espais (x-1))

output :: (a,b,c) -> a
output (x,_,_) = x

table :: (a,b,c) -> b
table (_,x,_) = x

input :: (a,b,c) -> c
input (_,_,x) = x

main = do
	tp <- getLine
	op <- getLine
	let tipus = read tp::Int
	    opcio = read op::Int
	    aux = "[]"
	    llistaINT = read aux::[Int]
	    llistaDUB = read aux::[Double]
	handle <- openFile "programhs.txt" ReadMode
	program <- hGetContents handle  
	g <- newStdGen
	if opcio == 0
		then do llistain <- getLine
			if tipus == 0
				then putStrLn $ opcioManual ((read llistain::[Int]) ++ randoms g) (read program :: Command Int)
				else putStrLn $ opcioManual ((read llistain::[Double]) ++ randoms g) (read program :: Command Double)
		else if opcio == 1
			then if tipus == 0
				then putStrLn $ opcioRandom (llistaINT++randoms g) (read program :: Command Int)
				else putStrLn $ opcioRandom (llistaDUB++randoms g) (read program :: Command Double)
			else do k <- getLine
				let tests = read k::Int
				if tipus == 0
					then putStrLn $ opcioBulk tests (llistaINT++randoms g) (read program :: Command Int)
					else putStrLn $ opcioBulk tests (llistaDUB++randoms g) (read program :: Command Double)
	hClose handle

opcioManual :: (Num a, Ord a, Show a) => [a] -> Command a -> String
opcioManual i program = case interpretProgram i program of
	Left s -> s
	Right x -> show x

opcioRandom :: (Num a, Ord a, Show a) => [a] -> Command a -> String
opcioRandom i com = "Entrada: " ++ show (input cosa) ++ "\n" ++ "Sortida: " ++ show (output cosa) ++ "\n" ++ "Instruccions: " ++ show (table cosa) ++ "\n" where cosa = interpretProgramAux i com

opcioBulk :: (Num a, Ord a, Show a) => Int -> [a] -> Command a -> String
opcioBulk k = opcioRandom


interpretAccumulatorAux :: (Num a, Ord a) =>  ((Either String [a],SymTable a, [a]), Int, [a]) -> Command a -> ((Either String [a],SymTable a, [a]), Int, [a])
interpretAccumulatorAux ((Left err, t, i),k,l) = ((Left err, t, i),k,l)
interpretAccumulatorAux acc com = case interpretCommandAux (table acc) (input acc) com of 
																					((Left err, _, _), inst, usedinput) -> ((Left err, table acc, input acc), (table acc)+inst, (input acc)++usedinput)
																					((Right l, t, i ), inst, usedinput) -> ((Right (outacc++l), t, i), (table acc)+inst, (input acc)++usedinput)
																					where Right outacc = output output acc



interpretCommandAux :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a],SymTable a, [a]), Int, [a])
interpretCommandAux t i (Assign s expr) = (asign t s expr i, 1, [])
interpretCommandAux t i (Input s) = (asign t s (Const (head i)) (tail i), 1, (head i):[])
interpretCommandAux t i (Print expr) = (printExpr t expr i,1, [])
interpretCommandAux t i (Empty s) = (empty t s i, 1, [])
interpretCommandAux t i (Push s expr) = (push t s expr i, 1, [])
interpretCommandAux t i (Pop s var) = (pop t s var i, 1, [])
interpretCommandAux t i (Size s var) = (size t s var i, 1, [])
interpretCommandAux t i (Seq l) = foldl (interpretAccumulatorAux) (((Right []),t,i), 0, []) l
interpretCommandAux t i (Cond expr thencom elsecom) = case eval (getValue t) expr of
													Right 1 -> interpretAccumulatorAux ((Right [],t,i), 0, []) thencom
													Right 0 -> interpretAccumulatorAux ((Right [],t,i), 0, []) elsecom
													Left x -> ((Left x, t, i),  0, [])
interpretCommandAux t i (Loop expr com) = case (eval (getValue t) expr) of
													Right 1 -> interpretAccumulatorAux (((Right outacc),(table cosa),(input cosa)),  0, []) (Loop expr com)
													Right 0 -> ((Right [], t, i),  0, [])
													Left x -> ((Left x, t, i),  0, [])
													where cosa = interpretAccumulatorAux ((Right [], t, i),  0, []) com
													      Right outacc = output cosa

interpretProgramAux :: (Num a, Ord a) => [a] -> Command a -> ((Either String [a]), Int, [a])
interpretProgramAux input com = case (interpretCommandAux TaulaBuida input com) of ((out, t, ileft), steps, iused) -> (out, steps, iused)
