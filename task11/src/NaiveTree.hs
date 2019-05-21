{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge Nil rt                  = rt
merge (Node key value l r) rt = Node key value l (merge r rt) 

{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton key value = Node key value Nil Nil 

    toAscList Nil                  = []
    toAscList (Node key value l r) = toAscList l ++ [(key, value)] ++ toAscList r

    alter f key Nil   = maybe Nil (singleton key) (f Nothing)
    alter f x (Node key value l r)
        | x < key     = Node key value (alter f x l) r
        | x > key     = Node key value l (alter f x r)
        | otherwise   = case f (Just value) of 
            Just fx -> merge l $ merge (Node x fx Nil Nil) r
            Nothing -> merge l r

    lookup _ Nil    = Nothing
    lookup x (Node key value l r)
        | x < key   = Map.lookup x l
        | x > key   = Map.lookup x r
        | otherwise = Just value

    null Nil = True
    null _   = False

    size Nil            = 0
    size (Node _ _ l r) = size l + 1 + size r
