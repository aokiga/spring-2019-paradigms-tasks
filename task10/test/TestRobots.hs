import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        robocop = robot "RoboCop" 20 110
        skeleton = robot "skeleton" 1000 0
        warrior = robot "warrior" 50 50
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        
        , testCase "Test for getAttack" $
            getAttack robocop @?= 20
        
        , testCase "Test for getHealth" $
            getHealth robocop @?= 110
        
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        
        , testCase "Test for setName" $
            getName (setName "test" robocop) @?= "test"
        
        , testCase "Test for setAttack" $
            getAttack (setAttack 1000 robocop) @?= 1000
        
        , testCase "Test for setHealth" $
            getHealth (setHealth 0 robocop) @?= 0

        , testCase "Test for damage" $
            damage robocop 50 @?= robot "RoboCop" 20 60

        , testCase "Test for isAlive" $
            isAlive robocop @?= True

        , testCase "Test for isAlive with dead" $
            isAlive skeleton @?= False

        , testCase "Test for isAlive" $
            isAlive robocop @?= True

        , testCase "Test for fight" $
            fight warrior robocop @?= robot "RoboCop" 20 60

        , testCase "Test for nRoundFight" $
            nRoundFight 6 robocop warrior @?= robot "RoboCop" 20 10

        , testCase "Test for threeRoundFight" $
            threeRoundFight robocop warrior @?= robot "RoboCop" 20 60

        , testCase "Test for neueRobotAttak" $
            neueRobotAttak robocop @?= robot "RoboCop" 20 101

        , testCase "Test for survivors" $
            survivors @?= [robot "The Star" 10 10]

        ]
