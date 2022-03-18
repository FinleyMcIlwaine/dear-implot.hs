{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module DearImGui.Plot.Structs where

-- base
import Data.Word
  ( Word32
#ifndef IMGUI_USE_WCHAR32
  , Word16
#endif
  )

import Foreign
  ( Storable(..), castPtr, plusPtr )

-- | DearImPlot context handle
data ImPlotContext

-- |  Double precision version of ImVec2 used by ImPlot. Extensible by end users
data ImPlotPoint

-- | Range defined by a min/max value.
data ImPlotRange

-- | Combination of two range limits for X and Y axes. Also an AABB defined by Min()/Max().
data ImPlotRect

-- | Plot style structure
data ImPlotStyle

-- | Input mapping structure. Default values listed. See also MapInputDefault, MapInputReverse.
data ImPlotInputMap

