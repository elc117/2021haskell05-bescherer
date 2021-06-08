--01
bmi :: Float -> Float -> String
bmi peso altura = 
    let x = altura^2
        total = peso/x
    in if total <= 18.5 then "ABAIXO" else if total >= 30.0 then "ACIMA" else "NORMAL"

--02
bmi' :: Float -> Float -> String
bmi' peso altura = if total <= 18.5 then "ABAIXO" else if total >= 30.0 then "ACIMA" else "NORMAL"
        where x = altura^2
              total = peso/x


--03.1
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

--03.2
cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
      let expr = ((sum $ zipWith (*) digits mults) `mod` 11)
      in if expr < 2 then 0 else 11-expr

--04
andTable :: [(Bool, Bool, Bool)]
andTable = [(boolX, boolY, boolX && boolY) | boolX <- res, boolY <- res]
  where res = [True,False]

main :: IO()
main = do
  putStr "CPF: "
  cpf <- getLine
  putStrLn cpf
