
asign :: (Num a, Ord a) => SymTable a -> Ident -> NExpr a -> ((Either String [a]),SymTable a, [a])

throwErr :: (Num a, Ord a) => SymTable a -> [a] -> String -> ((Either String [a]),SymTable a, [a])

printExpr :: (Num a, Ord a) => SymTable a -> NExpr a -> ((Either String [a]),SymTable a, [a])

empty :: (Num a, Ord a) => SymTable a -> Ident a -> ((Either String [a]),SymTable a, [a])

push :: (Num a, Ord a) => SymTable a -> Ident -> NExpr a -> ((Either String [a]),SymTable a, [a])

pop :: (Num a, Ord a) => SymTable a -> Ident -> Ident -> ((Either String [a]),SymTable a, [a])

size :: (Num a, Ord a) => SymTable a -> Ident -> Ident -> ((Either String [a]),SymTable a, [a])


interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]),SymTable a, [a])
interpretCommand t i (Asssign s expr) = asign t s expr
interpretCommand t i (Input s)
  | null i = throwErr "empty stack" t i
  | otherwise = asign t s (Const (head i))
interpretCommand t i (Print expr) = printExpr t expr
interpretCommand t i (Empty s) = empty t s
interpretCommand t i (Push s expr) = push t s expr
interpretCommand t i (Pop s var) = pop t s var
interpretCommand t i (Size s var) = size t s var
interpretCommand t i (Seq l) = foldl () 
interpretCommand t i (Cond expr thencom elsecom)
interpretCommand t i (Loop expr com)