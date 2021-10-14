module Wallet.Emulator.Stream.Extra (spanUntilSlot, takeUntilSlot') where

import Control.Lens (preview)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Log (LogMessage, logMessageContent)
import Streaming.Prelude qualified as S
import Prelude

import Ledger.Slot (Slot)
import Wallet.Emulator (EmulatorEvent)
import Wallet.Emulator.Chain (_SlotAdd)
import Wallet.Emulator.MultiAgent (chainEvent, eteEvent)

spanUntilSlot ::
  forall effs a.
  Slot ->
  S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a ->
  S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) (S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a)
spanUntilSlot maxSlot =
  S.span (maybe True (<= maxSlot) . preview (logMessageContent . eteEvent . chainEvent . _SlotAdd))

takeUntilSlot' ::
  forall effs a.
  Slot ->
  S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a ->
  S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
takeUntilSlot' maxSlot = S.drained . spanUntilSlot maxSlot
