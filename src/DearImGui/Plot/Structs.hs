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

--------------------------------------------------------------------------------
data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec2 where
  sizeOf ~ImVec2{x, y} = sizeOf x + sizeOf y

  alignment _ = 0

  poke ptr ImVec2{ x, y } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    return ImVec2{ x, y  }


data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec3 where
  sizeOf ~ImVec3{x, y, z} = sizeOf x + sizeOf y + sizeOf z

  alignment _ = 0

  poke ptr ImVec3{ x, y, z } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    return ImVec3{ x, y, z }


data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec4 where
  sizeOf ~ImVec4{x, y, z, w} = sizeOf x + sizeOf y + sizeOf z + sizeOf w

  alignment _ = 0

  poke ptr ImVec4{ x, y, z, w } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z
    poke (castPtr ptr `plusPtr` (sizeOf x * 3)) w

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    w <- peek (castPtr ptr `plusPtr` (sizeOf x * 3))
    return ImVec4{ x, y, z, w }

--------------------------------------------------------------------------------

-- | DearImGui context handle.
data ImGuiContext

-- | Individual font handle.
data ImFont

-- | Font configuration handle.
data ImFontConfig

-- | Glyph ranges builder handle.
data ImFontGlyphRangesBuilder

-- | Opaque DrawList handle.
data ImDrawList

-- | 'DearImGui.Raw.ListClipper.ListClipper' pointer tag.
data ImGuiListClipper

-- | A unique ID used by widgets (typically the result of hashing a stack of string)
--   unsigned Integer (same as ImU32)
type ImGuiID = Word32

-- | 32-bit unsigned integer (often used to store packed colors).
type ImU32 = Word32

type ImS16 = Int16

-- | Single wide character (used mostly in glyph management)
#ifdef IMGUI_USE_WCHAR32
type ImWchar = Word32
#else
type ImWchar = Word16
#endif

--------------------------------------------------------------------------------

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

