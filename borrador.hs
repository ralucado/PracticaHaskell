main = do
	tipus <- getLine
	opcio <- getLine
	handle <- openFile "programhs.txt" ReadMode
	program <- hGetContents handle  
	g <- newStdGen
	if opcio == 1
	then if tipus == 0
		 then putStrLn $ opcioManual ((read llistain::[Int]) ++ randoms g) (read program :: Command Int)
		 else putStrLn $ opcioManual ((read llistain::[Double]) ++ randoms g) (read program :: Command Double)
	else putStrLn "no valid option"
	hClose handle

opcioManual :: (Num a, Ord a) => [a] -> Command a -> String
opcioManual input program = case interpretProgram input program of
	Left s -> s
	Right x -> show x