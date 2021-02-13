{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2020-2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Numeric.PRIMME.Context
  ( -- | Provides "Language.C.Inline.Context" with type mappings specific to
    -- PRIMME library.
    primmeCtx,
  )
where

import qualified Data.Map as Map
import Foreign.C.Types (CInt)
import Language.C.Inline.Context (Context (..))
import qualified Language.C.Types as Types
import qualified Language.Haskell.TH as TH
import Numeric.PRIMME.Types

primmeTypesTable :: Map.Map Types.TypeSpecifier TH.TypeQ
primmeTypesTable =
  Map.fromList
    [ (Types.TypeName "primme_params", [t|Cprimme_params|]),
      (Types.TypeName "primme_target", [t|CInt|]),
      (Types.TypeName "primme_event", [t|CInt|]),
      (Types.TypeName "primme_preset_method", [t|CInt|]),
      (Types.TypeName "PRIMME_INT", [t|PrimmeInt|])
    ]

primmeCtx :: Context
primmeCtx = mempty {ctxTypesTable = primmeTypesTable}
