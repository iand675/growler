{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.Parsable where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.Lazy        as TL

-- | Minimum implemention: 'parseParam'
class Parsable a where
    -- | Take a 'ByteString' value and parse it as 'a', or fail with a message.
    parseParam :: BS.ByteString -> Either BS.ByteString a

    -- | Default implementation parses comma-delimited lists.
    --
    -- > parseParamList t = mapM parseParam (BS.split ',' t)
    parseParamList :: BS.ByteString -> Either BS.ByteString [a]
    parseParamList t = mapM parseParam (BS.split ',' t)

-- No point using 'read' for Text, ByteString, Char, and String.
instance Parsable T.Text where parseParam = Right . T.decodeUtf8
instance Parsable TL.Text where parseParam = Right . TL.fromStrict . T.decodeUtf8
instance Parsable BS.ByteString where parseParam = Right
-- | Overrides default 'parseParamList' to parse String.
instance Parsable Char where
    parseParam t = case T.unpack $ T.decodeUtf8 t of
                    [c] -> Right c
                    _   -> Left "parseParam Char: no parse"
    parseParamList = Right . T.unpack . T.decodeUtf8 -- String
-- | Checks if parameter is present and is null-valued, not a literal '()'.
-- If the URI requested is: '/foo?bar=()&baz' then 'baz' will parse as (), where 'bar' will not.
instance Parsable () where
    parseParam t = if BS.null t then Right () else Left "parseParam Unit: no parse"

instance (Parsable a) => Parsable [a] where parseParam = parseParamList

instance Parsable Bool where
    parseParam t = if t' == T.toCaseFold "true"
                   then Right True
                   else if t' == T.toCaseFold "false"
                        then Right False
                        else Left "parseParam Bool: no parse"
        where t' = T.toCaseFold $ T.decodeUtf8 t

instance Parsable Double where parseParam = readEither
instance Parsable Float where parseParam = readEither
instance Parsable Int where parseParam = readEither
instance Parsable Integer where parseParam = readEither

-- | Useful for creating 'Parsable' instances for things that already implement 'Read'. Ex:
--
-- > instance Parsable Int where parseParam = readEither
readEither :: (Read a) => BS.ByteString -> Either BS.ByteString a
readEither t = case [ x | (x, "") <- reads (T.unpack $ T.decodeUtf8 t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"
