import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [2..] @?= 2

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on infinite list too" $
        take 100 (tail' [1..]) @?= [2..101]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 3 elements from infinite list" $
        take' 3 [1..] @?= [1,2,3]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops all elements from 3-element list" $
        drop' 100 [1,2,3] @?= []

    , testCase "drop' drops 100 elements from infinite list" $
        take 100 (drop' 100 [1..]) @?= take 100 [101..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only even numbers from 0 to infinity" $
        take 100 (filter' even [0..]) @?= take 100 [0,2..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' can be used with empty list" $
        foldl'' (*) 4 [] @?= 4

    , testCase "foldl'' can be used with asymmetric operation" $
        foldl'' (\x y -> x * x + y) 0 [1, 2, 3] @?= 12

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on empty lists as expected" $
        concat' [] [4,5,6] @?= [4..6]

    , testCase "concat' works with right infinite list as expected" $
        take 100 (concat' [1..3] [4..]) @?= [1..100]

    , testCase "concat' works with left infinite list as expected" $
        take 100 (concat' [1..] [4..5]) @?= [1..100]

    , testCase "concat' works on two infinite lists as expected" $
        take 100 (concat' [1..] [4..]) @?= [1..100]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
