module PhoneExercise () where

import Data.Char
import qualified Data.List as List
import Data.Maybe

data Key
  = Key
  { button :: Char,
    letters :: [Char]
  }

phone =
  [ Key
      { button = '1',
        letters = []
      },
    Key
      { button = '2',
        letters = "abc"
      },
    Key
      { button = '3',
        letters = "def"
      },
    Key
      { button = '4',
        letters = "ghi"
      },
    Key
      { button = '5',
        letters = "jkl"
      },
    Key
      { button = '6',
        letters = "mno"
      },
    Key
      { button = '7',
        letters = "pqrs"
      },
    Key
      { button = '8',
        letters = "tuv"
      },
    Key
      { button = '9',
        letters = "wxyz"
      },
    Key
      { button = '*',
        letters = "^"
      },
    Key
      { button = '0',
        letters = "+ "
      },
    Key
      { button = '#',
        letters = ".,"
      }
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

findKey :: [Key] -> Char -> Maybe Key
findKey [] c = Nothing
findKey (k : keys) c =
  if (button k == c) || elem c (letters k) then Just k else findKey keys c

reverseTaps :: [Key] -> Char -> [(Digit, Presses)]
reverseTaps keys c =
  if isUpper c
    then
      reverseTaps keys '*' ++ reverseTaps keys (toLower c)
    else
      let key = findKey keys c
       in case key of
            Nothing -> []
            Just k ->
              case List.elemIndex c $ letters k of
                Nothing -> [(button k, length (letters k) + 1)]
                Just n -> [(button k, n + 1)]


cellPhonesDead :: [Key] -> String -> [(Digit, Presses)]
cellPhonesDead keys s =
  concatMap (reverseTaps keys) s
