relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((x,y):xs) | x == y = False
                             | pertenece (x,y)xs == True = False
                             | otherwise = relacionesValidas xs

pertenece :: (String,String) -> [(String, String)] -> Bool
pertenece _ [] = False
pertenece (a,b) ((c,d):xs) | (a,b) == (c,d) = True
                           | (a,b) == inverso (c,d) = True
                           | otherwise = pertenece (a,b) xs

inverso :: (String,String) -> (String,String)
inverso (a,b) = (b,a)

personas :: [(String,String)] -> [String]
personas [("","")] = []
personas ((a,b):xs) | xs == [] = [a,b]
                    | perteneceString a xs == True && perteneceString b xs == True = personas xs
                    | perteneceString a xs == True = b : personas xs
                    | perteneceString b xs == True = a : personas xs
                    | otherwise = a : b : personas xs

perteneceString :: String -> [(String,String)] -> Bool
perteneceString _ [] = False
perteneceString n ((a,b):xs) | n == a || n == b = True
                             | xs == [] = False
                             | otherwise = perteneceString n xs

amigosDe :: String -> [(String,String)] -> [String]
amigosDe n [] = []
amigosDe n ((a,b):xs) | perteneceString n ((a,b):xs) == False = []
                      | darCompañero n ((a,b):xs) /= [] = darCompañero n ((a,b):xs) : amigosDe n xs 
                      | otherwise = []

darCompañero :: String -> [(String,String)] -> String
darCompañero _ [] = ""
darCompañero n ((a,b):xs) | xs == [] = ""
                          | n == a = b
                          | n == b = a
                          | otherwise darCompañero n xs

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = _
quitarRepetidos (x:xs) | pertenece x xs == True = quitarRepetidos xs
                       | otherwise = x : 