type Ident = String
data NExpr a = Var Ident
	     | Const a 
	     | Plus (NExpr a) (NExpr a) 
	     | Minus (NExpr a) (NExpr a) 
	     | Times (NExpr a) (NExpr a)
	     
data BExpr a = AND (BExpr a) (BExpr a)
	     | OR (BExpr a) (BExpr a)
	     | NOT (BExpr a) 
	     | Gt (NExpr a) (NExpr a) 
	     | Eq (NExpr a) (NExpr a)
	     
data Command a = Assign Ident (NExpr a)
	       | Input Ident
	       | Print (NExpr a)
	       | Empty Ident
	       | Push Ident (NExpr a)
	       | Pop Ident
	       | Size Ident
	       | Seq [Command a]
	       | Cond (BExpr a) (Command a)
	       | Loop (BExpr a) (Command a)