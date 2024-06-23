import Text.Trifecta
import Control.Applicative

{-
<valid semver> ::= <version core>
                 | <version core> "-" <pre-release>
                 | <version core> "+" <build>
                 | <version core> "-" <pre-release> "+" <build>

<version core> ::= <major> "." <minor> "." <patch>

<major> ::= <numeric identifier>

<minor> ::= <numeric identifier>

<patch> ::= <numeric identifier>

<pre-release> ::= <dot-separated pre-release identifiers>

<dot-separated pre-release identifiers> ::= <pre-release identifier>
                                          | <pre-release identifier> "." <dot-separated pre-release identifiers>

<build> ::= <dot-separated build identifiers>

<dot-separated build identifiers> ::= <build identifier>
                                    | <build identifier> "." <dot-separated build identifiers>

<pre-release identifier> ::= <alphanumeric identifier>
                           | <numeric identifier>

<build identifier> ::= <alphanumeric identifier>
                     | <digits>

<alphanumeric identifier> ::= <non-digit>
                            | <non-digit> <identifier characters>
                            | <identifier characters> <non-digit>
                            | <identifier characters> <non-digit> <identifier characters>

<numeric identifier> ::= "0"
                       | <positive digit>
                       | <positive digit> <digits>

<identifier characters> ::= <identifier character>
                          | <identifier character> <identifier characters>

<identifier character> ::= <digit>
                         | <non-digit>

<non-digit> ::= <letter>
              | "-"

<digits> ::= <digit>
           | <digit> <digits>

<digit> ::= "0"
          | <positive digit>

<positive digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
           | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
           | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d"
           | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n"
           | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
           | "y" | "z"
-}

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Prelude.Ord NumberOrString where
  compare (NOSS a) (NOSS b) = Prelude.compare a b
  compare (NOSI a) (NOSI b) = Prelude.compare a b
  compare (NOSS _) (NOSI _) = LT
  compare (NOSI _) (NOSS _) = GT

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 release1 metadata1) (SemVer major2 minor2 patch2 release2 metadata2)
    | major1 /= major2 = Prelude.compare major1 major2
    | minor1 /= minor2 = Prelude.compare minor1 minor2
    | patch1 /= patch2 = Prelude.compare patch1 patch2
    | release1 /= release2 = Prelude.compare release1 release2
    | otherwise = Prelude.compare metadata1 metadata2

parseCore :: Parser (Integer, Integer, Integer)
parseCore = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return (major, minor, patch)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOSS <$> some letter) <|> (NOSI <$> integer)

parseRelease :: Parser [NumberOrString]
parseRelease = sepBy1 parseNumberOrString (char '.')

parseMetadata :: Parser [NumberOrString]
parseMetadata = sepBy1 parseNumberOrString (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseCore
  release <- option [] $ char '-' >> parseRelease
  metadata <- option [] $ char '+' >> parseMetadata
  return $ SemVer major minor patch release metadata
