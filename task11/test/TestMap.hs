{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) = 
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "empty" [
            testCase "empty map" $ 
                let map = Map.empty :: m Int Int in
                Map.size map @?= 0
        ],

        testGroup "singleton" [
            testCase "singleton map" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.size map @?= 1
        ],

        testGroup "fromList" [
            testCase "make map from list" $
                let map = Map.fromList [(1, 2), (2, 3), (3, 4), (5, 6)] :: m Int Int in do
                    Map.size map     @?= 4
                    Map.lookup 1 map @?= Just 2
                    Map.lookup 2 map @?= Just 3
                    Map.lookup 3 map @?= Just 4
                    Map.lookup 5 map @?= Just 6
        ],

        testGroup "toAscList" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "insert" [
            testCase "key doesn't exist" $
                let map = Map.singleton 1 2 :: m Int Int in
                let map' = Map.insert 2 3 map in
                Map.lookup 2 map' @?= Just 3
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 123 :: m Int Int in
                let map' = Map.insert 1 321 map in
                Map.lookup 1 map' @?= Just 321
        ],

        testGroup "insertWith" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 2 2 :: m Int Int in
                let map' = Map.insertWith (+) 1 123 map in
                Map.lookup 1 map' @?= Just 123
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.insertWith (+) 1 123 map in
                Map.lookup 1 map' @?= Just 124
        ],

        testGroup "insertWithKey" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 2 2 :: m Int Int in
                let map' = Map.insertWithKey (\key new old -> key + new + old) 1 123 map in
                Map.lookup 1 map' @?= Just 123
            ,
            testCase "key exists" $
                let map  = Map.singleton 2 1 :: m Int Int in
                let map' = Map.insertWithKey (\key new old -> key + new + old) 2 123 map in
                Map.lookup 2 map' @?= Just 126
        ],

        testGroup "delete" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.delete 2 map in do
                    Map.size map'      @?= 1 
                    Map.lookup 1 map' @?= Just 1
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.delete 1 map in
                Map.null map' @?= True
        ],

        testGroup "adjust" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.adjust (7 +) 3 map in do
                    Map.size map'     @?= 1
                    Map.lookup 1 map' @?= Just 1
            ,
            testCase "key exists" $
                let map  = Map.singleton 2 3 :: m Int Int in
                let map' = Map.adjust (7 +) 2 map in
                Map.lookup 2 map' @?= Just 10
        ],

        testGroup "adjustWithKey" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.adjust (7 +) 3 map in do
                    Map.size map'     @?= 1
                    Map.lookup 1 map' @?= Just 1
            ,
            testCase "key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.adjust (7 +) 10 map in
                Map.lookup 10 map' @?= Just 8
        ],

        testGroup "update" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 2 :: m Int Int in
                let map' = Map.update (\x -> Just (x^2)) 3 map in do
                    Map.size map'     @?= 1 
                    Map.lookup 1 map' @?= Just 2
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 2 :: m Int Int in
                let map' = Map.update (\x -> Just (x^2)) 1 map in
                Map.lookup 1 map' @?= Just 4
            ,
            testCase "delete the key" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.update (const Nothing) 1 map in
                Map.null map' @?= True
        ],

        testGroup "updateWithKey" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 2 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Just (k + x^2)) 3 map in do
                    Map.size map'     @?= 1 
                    Map.lookup 1 map' @?= Just 2
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 2 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Just (k + x^2)) 1 map in
                Map.lookup 1 map' @?= Just 5
            ,
            testCase "delete the key" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Nothing) 1 map in
                Map.null map' @?= True
        ],

        testGroup "alter" [
            testCase "insert" $ 
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.alter (const $ Just 17) 2 map in
                Map.lookup 2 map' @?= Just 17
            ,
            testCase "alter the value" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.alter (const $ Just 17) 1 map in
                Map.lookup 1 map' @?= Just 17
            ,
            testCase "delete the key" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.alter (const Nothing) 1 map in
                Map.null map' @?= True
        ],

        testGroup "lookup" [
            testCase "key doesn't exist" $
                let map  = Map.singleton 1 2 :: m Int Int in
                Map.lookup 2 map @?= Nothing
            ,
            testCase "key exists" $
                let map  = Map.singleton 1 2 :: m Int Int in
                Map.lookup 1 map @?= Just 2
        ],

        testGroup "member" [
            testCase "key exists" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.member 1 map @?= True
            ,
            testCase "key doesn't exist" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.member 2 map @?= False
        ],

        testGroup "notMember" [
            testCase "key exists" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.notMember 1 map @?= False
            ,
            testCase "key doesn't exist" $
                let map = Map.singleton 1 2 :: m Int Int in
                Map.notMember 2 map @?= True
        ],

        testGroup "null" [
            testCase "empty map" $
                let map = Map.empty :: m Int Int in
                Map.null map @?= True
            ,
            testCase "non-empty map" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.null map @?= False
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
