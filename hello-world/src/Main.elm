module Main exposing (main)
import Html exposing (text)

main = text (bottlesOf "juice" 99)

greeting : String
greeting =  sayHello "Elm"

sayHello : String -> String
sayHello name = "Hallo, " ++ name ++ "."

meaningOfLife : Int
meaningOfLife = 42

pi : Float
pi = 3.14

canChuck : Bool
canChuck = True

bottlesOf : String -> Int -> String
bottlesOf contents amount = String.fromInt amount ++ " bottles of " ++ contents ++ " on the wall."

