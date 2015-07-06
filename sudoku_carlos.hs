le_sudoku x
| x < 1 = return []
| otherwise = do
y <- getLine >>= return . words >>= return . map (\z -> read z :: Int)
z <- le_sudoku (x-1)
return (y:z)

sudoku_ok [_] = True
sudoku_ok ([]:[]:[]:y) = sudoku_ok y
sudoku_ok ((a:b):(c:d):(e:f):x)
| a==c || a==e || c==e || (elem a b) || (elem c d) || (elem e f)  = False
| otherwise = sudoku_ok(b:d:f:x)

sudoime x = sudoku [1..x]
where sudoku [] = return []
     sudoku (y:z) = do
a <- le_sudoku 9
b <- sudoku z
return ((y,sudoku_ok a):b)