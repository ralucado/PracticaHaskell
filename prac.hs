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
	       | Pop Ident
	       | Size Ident
	       | Seq [Command a]
	       | Cond (BExpr a) (Command a) (Command a)
	       | Loop (BExpr a) (Command a)
	         deriving(Read, Show)

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
    

instance Show a => Show(Command a) where
    show = indenta 0 
    where 
      indenta x (Assign s expr) = "  "++ s ++ " := " ++ (show expr)