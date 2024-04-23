module TextSplitter where

import Text.Parsec
import Text.Parsec.Char

data SplitSource = SourceEnd | ContentItem Int String SplitSource | ContentSeparator String SplitSource

instance Show SplitSource where
    show SourceEnd = ""
    show (ContentItem n s rest) = "Item #" ++ show n ++ ": " ++ s ++ "\n" ++ show rest
    show (ContentSeparator _ rest) = show rest

includedTerminators = ['.', ':', '"','\'']
excludedTerminators = [' ', '\n', '\r']
guaranteedTerminators = ['\n', '\r']

contentMarker = "@@@"

parseContentSeparator :: Parsec String () String
parseContentSeparator = many space <|> (eof >> return "")

parseContentItem :: Parsec String () String
parseContentItem
    = do
        body <- manyTill anyChar (eof <|> (try $ lookAhead $ (parseTerminator >> return ())))
        ending <- option "" (fmap (:[]) parseIncludedTerminator)
        return (body ++ ending)
    where
        parseIncludedTerminator = oneOf includedTerminators
        parseExcludedTerminator = oneOf excludedTerminators
        parseGuaranteedTerminator = oneOf guaranteedTerminators
        parseTerminator = parseGuaranteedTerminator <|> (parseIncludedTerminator >> parseExcludedTerminator)

-- TODO: would it make more sense for this to have a state of Int?
parseSplitSource :: Parsec String () SplitSource
parseSplitSource = do
    items <- many parseOne
    let indItems = zip items [0..]
    let splitSrc = foldr
                    (\((sp, itm), ind) rest -> ContentSeparator sp $ ContentItem ind itm rest) 
                    SourceEnd 
                    indItems
    return splitSrc
    where
        parseOne = do
            contSep <- parseContentSeparator
            contItem <- parseContentItem
            if contSep == "" && contItem == "" then parserFail "" else return (contSep, contItem)

parseContentFile :: Parsec String () [String]
parseContentFile = many $ do
    optional (char '\n')
    string contentMarker
    many digit
    char ' '
    many (noneOf ['\n'])

reassembleSourceTemplate :: [String] -> Parsec String () String
reassembleSourceTemplate content = do
    markers <- many parseOne
    let reassembled = foldr (\(sp, itm) rest -> sp ++ itm ++ rest) "" markers
    return reassembled
    where
        parseOne = do
            contSep <- parseContentSeparator
            contMarker <- fmap ((content !!) . read) (string contentMarker >> many digit)
            return (contSep, contMarker)

splitSourceContent :: SplitSource -> String
splitSourceContent src = tail $ go src ""
    where
        go SourceEnd acc = acc
        go (ContentItem n s rest) acc = go rest (acc ++ "\n" ++ contentMarker ++ show n ++ " " ++ s)
        go (ContentSeparator _ rest) acc = go rest acc

splitSourceTemplate :: SplitSource -> String
splitSourceTemplate src = go src ""
    where
        go SourceEnd acc = acc
        go (ContentItem n _ rest) acc = go rest (acc ++ contentMarker ++ show n)
        go (ContentSeparator s rest) acc = go rest (acc ++ s)
