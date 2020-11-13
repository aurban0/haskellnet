valor :: Char -> Int
valor 'I' = 1
valor 'V' = 5
valor 'X' = 10
valor 'L' = 50
valor 'C' = 100
valor 'D' = 500
valor 'M' = 1000

romanoADecimal :: String -> Int
romanoADecimal xs | length xs == 1 = valor (xs!!0)
                  | length xs == 2 = if valor (xs!!1) <= valor (xs!!0) then (valor (xs!!1)) + (valor (xs!!0)) else (valor (xs!!1)) - (valor (xs!!0))
                  | valor (last xs) <= valor (last (init xs)) = (valor (last xs)) + (romanoADecimal (init xs))
                  | valor (last xs) > valor (last (init xs)) = (valor (last xs)) - (valor (last (init xs))) + (romanoADecimal (init (init xs)))
