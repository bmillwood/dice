{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dice (
  DiceRoll(..),
  DiceRoller(..),
  DiceSpec(..),
  Dicetabase(..),
  withState
 ) where

import Control.Applicative ((<$), (<$>), pure)
import Control.Category (id, (.))
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
  specDice :: Integer,
  specFaces :: Integer }
 deriving (Show, Typeable)
type RollResult = Integer
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

fetchRollerQ :: T.Text -> Query DiceState (Maybe DiceRoller)
fetchRollerQ name = asks (M.lookup name . unDS)

-- | Return False if roller already exists
registerRollerU :: T.Text -> L.ByteString -> UTCTime -> Update DiceState Bool
registerRollerU name pwhash date =
  maybe (True <$ reg) (const (pure False)) =<< runQuery (fetchRollerQ name)
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
  update Nothing = pure False
  update (Just r) 
    | pwhash /= rollerPW r = pure False
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
    rollFor = \text pw reason spec@MkSpec{ specDice, specFaces } -> do
      now <- getCurrentTime
      res <- sum <$> replicateM (fromInteger specDice)
        (randomRIO (1, specFaces))
      authed <- update h $ RollForU text (enhashen pw) MkRoll{
        rollReason = reason,
        rollTime = now,
        rollSpec = spec,
        rollResult = res }
      pure $ res <$ guard authed }
  closeAcidState h
  return res
