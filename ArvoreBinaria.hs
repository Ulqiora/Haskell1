data Arv a = No a (Arv a) (Arv a)
           | Vazia

pertence :: a -> Arv a -> Bool
pertence x Vazia = False
pertence x (No y esq dir) | x == y = True
                          | x < y = pertence x esq
                          | otherwise = pertence x dir

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

inserir :: a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir) | x == y = No y esq dir
                         | x < y = No y (inserir x esq) dir
                         | otherwise = No y esq (inserir x dir)

fromList :: [a] -> Arv a
fromList [] = Vazia
fromList (x :xs) = inserir x (fromList xs)

remover :: a -> Arv a -> Arv a
remove x Vazia = Vazia
remove x (No y esq dir) | x < y = No y (remove x esq) dir
                        | x > y = No y esq (remove x dir)
                        | dir == Vazia = esq
                        | esq == Vazia = dir
                        | otherwise = No z (remove z esq) (dir)
                        where z = mais_dir (esq)

--data Arv = No Arv Arv
--         | Folha

--(Int->Int->Int)
--data ArvExp = No _ ArvExp ArvExp
 --           | Folha Int

-- No (+) (No (*) (Folha 4) (folha 3)) (No (-) (Folha 5) (Folha 2))

--data Arv a = No (Arv a) (Arv a)
--           | Folha a

--data Arv a b = No (Arv b) (Arv b)
  --           | Folha a
