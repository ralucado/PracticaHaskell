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
