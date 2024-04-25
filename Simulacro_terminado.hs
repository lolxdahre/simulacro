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
                      | darCompañero n ((a,b):xs) /= [] = quitarRepetidos (darCompañero n ((a,b):xs) : amigosDe n xs) 
                      | otherwise = []

darCompañero :: String -> [(String,String)] -> String
darCompañero _ [] = ""
darCompañero n ((a,b):xs) | n == a = b
                          | n == b = a
                          | xs == [] = ""
                          | otherwise = darCompañero n xs

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | xs == [""] = [x]
                       | estaRepetido x xs == True = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs 

estaRepetido :: String -> [String] -> Bool
estaRepetido _ [] = False
estaRepetido a (x:xs) | a == x = True
                      | xs == [] = False
                      | otherwise = estaRepetido a xs

-- a partir de la lista con los elementos de cada tupla con mas apariciones, selecciona entre esos cual es el que mas apariciones tiene
personaConMasAmigos :: [(String,String)]-> String
personaConMasAmigos [] = []
personaConMasAmigos ((a,b):xs) =  (masApariciones (nominados ((a,b):xs) 0)) 

--arma una nueva lista con los elementos de cada tupla con mas apariciones 
nominados :: [(String,String)] -> Int -> [String]
nominados [] _ = []
nominados ((a,b):xs) n | n == length ((a,b):xs) = []
                       | cantidadDeApariciones a ((a,b):xs) > cantidadDeApariciones b ((a,b):xs) = a : nominados (xs ++ [(a,b)]) (n+1)
                       | otherwise =  b : nominados (xs ++ [(a,b)]) (n+1)

--a partir de una lista, decide cual es el que tiene mas apariciones
masApariciones :: [String] ->  String
masApariciones [] = ""
masApariciones (x:xs) | cuantosHay x (x:xs) == length (x:xs) = x
                      | x == head xs = masApariciones (xs ++ [x])
                      | cuantosHay x xs <= cuantosHay (head (xs)) (tail (xs)) = masApariciones xs 
                      | cuantosHay x xs > cuantosHay (head (xs)) (tail (xs)) = masApariciones ((tail (xs) ++ [x]))

-- cuenta las apariciones de un elemento en una lista de tuplas
cantidadDeApariciones :: String -> [(String,String)] -> Int
cantidadDeApariciones _ [] = 0
cantidadDeApariciones n ((a,b):xs) | n == a || n == b = 1 + cantidadDeApariciones n xs
                                   | xs == [] = 0
                                   | otherwise = cantidadDeApariciones n xs

-- cuenta las apariciones de un elemento en una lista de strings
cuantosHay :: String -> [String] -> Int
cuantosHay _ [] = 0
cuantosHay n (x:xs) | n == x = 1 + cuantosHay n (xs)
                    | xs == [] = 0
                    | otherwise = cuantosHay n (xs)