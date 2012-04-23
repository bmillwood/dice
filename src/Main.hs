{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((<=<))
import qualified Data.ByteString as S
import System.Environment (getArgs, getEnvironment)
import qualified Data.Foldable as F
import Data.Monoid (mempty)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Prelude hiding (head)
import Text.Blaze.Renderer.Text (renderHtml)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Types (parseQueryText)

import Dice

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn "I need somebody, not just anybody, etc."
    ["--version"] -> putStrLn "Internet dice roller, version ω"
    [] -> putHtml =<< either return display =<< handlePost
    _ -> putStrLn "I don't want any arguments :("
 where
  display a = maybe (pure noQuery) (mkPage a) =<<
    envToPerson <$> getEnvironment
  envToPerson = assocsToPerson <=< parseSimpleQueryText . toBS <=<
    lookup "QUERY_STRING"
  -- there are possibly more direct ways to do this, but the text
  -- library does it without adding any dependencies:
  toBS = T.encodeUtf8 . T.pack

parseSimpleQueryText :: S.ByteString -> Maybe [(T.Text,T.Text)]
parseSimpleQueryText = mapM (\(a,b) -> (,) a <$> b) . parseQueryText

noQuery :: Html
noQuery = do
  mkHead "Huh?"
  body . p . text . T.concat $ [
    "The query string doesn't look right. If this happens a lot, ",
    "let me know and I'll write better error handling."]

putHtml :: Html -> IO ()
putHtml = TL.putStr .
  TL.append "Content-Type: text/html; charset=UTF-8\n\n" .
  renderHtml . docTypeHtml

-- | Left: a complete error page; Right: prepend to body
handlePost :: IO (Either Html Html)
handlePost = do
  postData <- parseSimpleQueryText <$> S.getContents
  let
    parseFailure = pure . Left $ do
      mkHead "POST data parse failure"
      body $ do
        p . text $ "The POST data didn't parse. This is a bit surprising."
        p . toHtml . show $ postData
  case postData of
    Just [("registerName",n),("registerPassword",p),("registerConfirm",c)]
      | p /= c -> prepend $ "Your passwords don't match!"
      | otherwise -> withState $ \DB{ registerRoller } -> do
          success <- registerRoller n p
          prepend . T.concat $ if success
            then ["Registration complete for: ", n]
            else [n, " already exists, sorry!"]
      where
    Just [("rollName",n),("rollPassword",p),("rollReason",r),
        ("times",t),("dice",d),("faces",f),("mod",m)] ->
      prepend =<< maybe (pure "I don't think that's a number.") checkAuth
        (readSpec t d f m)
     where
      readSpec t d f m = pure MkSpec <*+> t <*+> d <*+> f <*+> m
      f <*+> x = f <*> case reads (T.unpack x) of
        [(r,"")] -> Just r
        _ -> Nothing
      checkAuth spec = withState $ \DB{ rollFor } ->
        maybe
          "Authentication failed!"
          (T.append "You rolled: " . T.pack . show) <$>
          rollFor n p r spec
    Just [] -> pure (Right mempty)
    _ -> parseFailure
 where
  prepend = pure . Right . p . text

-- This function is nearly all pure. Should factor out the IO that does happen.
mkPage :: Html -> Maybe T.Text -> IO Html
mkPage prep Nothing = pure $ do
  mkHead "Dice rolls!"
  body $ do
    prep
    rollerSelectForm
    rollSubmitForm Nothing
    rollerRegisterForm
mkPage prep (Just person) = withState $ \DB{ fetchRoller } -> do
  roller <- fetchRoller person
  pure $ case roller of
    Nothing -> do
      mkHead "Dice rolls for nobody"
      body $ do
        prep
        p (text (T.append person " isn't a dice roller!"))
        rollerSelectForm
    Just MkRoller{ rollerRolls = rs } -> do
      mkHead (T.append "Dice rolls for: " person)
      body $ do
        prep
        p (text (T.append "Dice rolls for " person))
        table $
          F.forM_ rs $ \MkRoll{ rollSpec, rollResult, rollReason } -> tr $ do
            td (specToHtml rollSpec)
            td (text . resultText $ rollResult)
            td (text rollReason)
        rollSubmitForm (Just person)

specToHtml :: DiceSpec -> Html
specToHtml MkSpec{ specTimes, specDice, specFaces, specMod } = do
  toHtml specTimes
  text "#"
  toHtml specDice
  text "d"
  toHtml specFaces
  text "+"
  toHtml specMod

rollerSelectForm :: Html
rollerSelectForm = form ! A.method "get" ! A.id "rollerSelect" $ do
  text "See dice rolls for: "
  textInput formKey
  submitButton "Fetch!"

rollSubmitForm :: Maybe T.Text -> Html
rollSubmitForm defaultPerson = form ! A.method "post" ! A.id "rollSubmit" $ do
  p (text "Roll a die:")
  table . mapM_ (\(t,i) -> tr (td (text t) >> td i)) $ [
    ("Name:", nameInput),
    ("Password:", passwordInput "rollPassword"),
    ("Reason:", textInput "rollReason"),
    ("Roll:", do
      textInput "times" ! A.id "timesInput" ! A.value "1"
      text "#"
      textInput "dice" ! A.id "diceInput"
      text "d"
      textInput "faces" ! A.id "facesInput"
      text "+"
      textInput "mod" ! A.id "modInput" ! A.value "0")]
  submitButton "Roll!"
 where
  nameInput = maybe id (flip (!) . A.value . toValue) defaultPerson $
    textInput "rollName"

rollerRegisterForm :: Html
rollerRegisterForm = form ! A.method "post" ! A.id "rollerRegister" $ do
  p (text "Register yourself:")
  table . mapM_ (\(t,i) -> tr (td (text t) >> td i)) $ [
    ("Name:", textInput "registerName"),
    ("Password:", passwordInput "registerPassword"),
    ("Retype:", passwordInput "registerConfirm")]
  submitButton "Make me a roller!"

textInput :: AttributeValue -> Html
textInput name = input ! A.type_ "text" ! A.name name

passwordInput :: AttributeValue -> Html
passwordInput name = input ! A.type_ "password" ! A.name name

submitButton :: AttributeValue -> Html
submitButton value = input ! A.type_ "submit" ! A.value value

mkHead :: T.Text -> Html
mkHead t = head $ do
  title (text t)
  link ! A.rel "stylesheet" ! A.href "dice.css"

-- This doesn't really /need/ to be factored out, but it helps me remember
-- that if I want to change it I should do so in two places
formKey :: (IsString s) => s
formKey = "person"

-- Maybe Maybe is a bit silly but it kind of makes sense
-- Nothing -> bad format
-- Just Nothing -> good format, but no person
-- Just (Just someone) -> someone.
assocsToPerson :: [(T.Text,T.Text)] -> Maybe (Maybe T.Text)
assocsToPerson [] = Just Nothing
assocsToPerson [(k,v)] | k == formKey = Just (Just v)
assocsToPerson _ = Nothing
