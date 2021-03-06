{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dice (
  DiceRoll(..),
  DiceRoller(..),
  DiceSpec(..),
  Dicetabase(..),
  resultText,
  withState
 ) where

import Control.Applicative ((<$), (<$>))
import Control.Category ((.))
import Control.Monad (guard, replicateM)
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Sequence as Q
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Prelude hiding (id, (.))

import Data.Acid (Query, Update,
    closeAcidState, makeAcidic, openLocalState, query, runQuery, update)
import Data.Digest.Pure.SHA (sha256, bytestringDigest)
import Data.Lens.Common (Lens, setL, modL, iso, lens, mapLens)
import Data.SafeCopy (base, deriveSafeCopy)
import System.Random (randomRIO)

data DiceSpec = MkSpec {
  specTimes :: Integer,
  specDice :: Integer,
  specFaces :: Integer,
  specMod :: Integer }
 deriving (Show, Typeable)
type RollResult = [Integer]
data DiceRoll = MkRoll {
  rollReason :: T.Text,
  rollTime :: UTCTime,
  rollSpec :: DiceSpec,
  rollResult :: RollResult }
 deriving (Show, Typeable)
data DiceRoller = MkRoller {
  rollerName :: T.Text,
  rollerPW :: L.ByteString, -- hashed
  rollerDate :: UTCTime,
  rollerRolls :: Q.Seq DiceRoll }
 deriving (Show, Typeable)
newtype DiceState = MkDS { unDS :: M.Map T.Text DiceRoller }
 deriving (Show, Typeable)
data Dicetabase = DB {
  fetchRoller :: T.Text -> IO (Maybe DiceRoller),
  registerRoller :: T.Text -> T.Text -> IO Bool,
  rollFor :: T.Text -> T.Text -> T.Text -> DiceSpec -> IO (Maybe RollResult) }

rollerL :: T.Text -> Lens DiceState (Maybe DiceRoller)
rollerL name = mapLens name . iso unDS MkDS

rollerRollsL :: Lens DiceRoller (Q.Seq DiceRoll)
rollerRollsL = lens rollerRolls (\rs r -> r { rollerRolls = rs })

-- This needn't be in IO but in practice that's where it will be used.
rollDice :: DiceSpec -> IO RollResult
rollDice MkSpec{ specTimes, specDice, specFaces, specMod } =
  replicateM (fromInteger specTimes) roll
 where
  roll = (specMod +) . sum <$>
    replicateM (fromInteger specDice) (randomRIO (1, specFaces))

-- ought to build the Text directly without using pack, but whatever.
resultText :: RollResult -> T.Text
resultText = T.unwords . map (T.pack . show)

fetchRollerQ :: T.Text -> Query DiceState (Maybe DiceRoller)
fetchRollerQ name = asks (M.lookup name . unDS)

-- | Return False if roller already exists
registerRollerU :: T.Text -> L.ByteString -> UTCTime -> Update DiceState Bool
registerRollerU name pwhash date =
  maybe (True <$ reg) (const (return False)) =<< runQuery (fetchRollerQ name)
 where
  reg = modify (setL (rollerL name) (Just newRoller))
  newRoller = MkRoller {
    rollerName = name,
    rollerPW = pwhash,
    rollerDate = date,
    rollerRolls = Q.empty }

-- | Return False if authentication failed (including if roller doesn't exist)
rollForU :: T.Text -> L.ByteString -> DiceRoll -> Update DiceState Bool
rollForU name pwhash roll = runQuery (fetchRollerQ name) >>= update
 where
  update Nothing = return False
  update (Just r) 
    | pwhash /= rollerPW r = return False
    | otherwise = True <$ modify (setL (rollerL name)
        (Just (modL rollerRollsL (roll Q.<|) r)))

enhashen :: T.Text -> L.ByteString
enhashen = bytestringDigest . sha256 . L.fromChunks . (:[]) . T.encodeUtf8

-- TH puts requirements on the order we declare things, this makes me sad :(
fmap concat . mapM (deriveSafeCopy 1 'base) $
  [''DiceSpec, ''DiceRoll, ''DiceRoller, ''DiceState]
makeAcidic ''DiceState
  ['fetchRollerQ, 'registerRollerU, 'rollForU]

withState :: (Dicetabase -> IO a) -> IO a
withState fn = do
  h <- openLocalState (MkDS M.empty)
  res <- fn $ DB {
    fetchRoller = query h . FetchRollerQ,
    registerRoller = \name pw -> do
      now <- getCurrentTime
      update h (RegisterRollerU name (enhashen pw) now),
    rollFor = \name pw reason spec -> do
      now <- getCurrentTime
      res <- rollDice spec
      authed <- update h $ RollForU name (enhashen pw) MkRoll{
        rollReason = reason,
        rollTime = now,
        rollSpec = spec,
        rollResult = res }
      return $ res <$ guard authed }
  closeAcidState h
  return res
