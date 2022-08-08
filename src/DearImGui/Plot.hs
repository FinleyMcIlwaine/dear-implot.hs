{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.Plot

Main ImPlot module, exporting the functions to make plots happen in Gui.
-}

module DearImGui.Plot
  ( -- * Context Creation and Access
    Raw.Plot.PlotContext(..)
  , Raw.Plot.createPlotContext
  , Raw.Plot.destroyPlotContext
  , Raw.Plot.getCurrentPlotContext
  , Raw.Plot.setCurrentPlotContext

    -- * Demo so you can play with all features
  , Raw.Plot.showPlotDemoWindow

    -- * Plot Creation
  , withPlot
  , setupAxisLimits

    -- * TEST
  , plotLine
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Bool
import Data.Foldable
  ( foldl' )
import Foreign
import Foreign.C
import qualified GHC.Foreign as Foreign
import System.IO
  ( utf8 )

-- dear-imgui
import DearImGui.Enums
import DearImGui.Structs
import DearImGui.Plot.Enums
import DearImGui.Plot.Structs
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.Plot as Raw.Plot
import qualified DearImGui.Raw.Font as Raw.Font
import qualified DearImGui.Raw.ListClipper as Raw.ListClipper

-- managed
import qualified Control.Monad.Managed as Managed

-- StateVar
import Data.StateVar
  ( HasGetter(get), HasSetter, ($=!) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- unliftio
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, bracket_)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

plotLine :: (MonadIO m) => String -> VS.Vector Float -> VS.Vector Float -> m ()
plotLine label xs ys = liftIO $ do
  let (xsPtr, xsLen) = VS.unsafeToForeignPtr0 xs
      (ysPtr, ysLen) = VS.unsafeToForeignPtr0 ys
  when (xsLen /= ysLen) $ error $ "Vectors have not equal length: x " <> show xsLen <> " /= y " <> show ysLen
  withCString label \labelPtr -> do
    withForeignPtr xsPtr $ \xsPtr' -> do
      withForeignPtr ysPtr $ \ysPtr' -> do
        -- CFloat = CFloat Float -> ptr-cast is no problem
        Raw.Plot.plotLine labelPtr (castPtr xsPtr') (castPtr ysPtr') (fromIntegral xsLen)

withPlot :: (MonadIO m) => String -> m () -> m ()
withPlot p a = Raw.Plot.beginPlot p >>= \case
    False -> return ()
    True -> a >> Raw.Plot.endPlot

setupAxisLimits :: MonadIO m => (Double, Double) -> (Double, Double) -> Maybe Int -> m ()
setupAxisLimits (minX, maxX) (minY, maxY) _ = liftIO $ do
  Raw.Plot.setupAxisLimits ImAxis_X1 minX' maxX' Nothing
  Raw.Plot.setupAxisLimits ImAxis_Y1 minY' maxY' Nothing
  where
    minX' = realToFrac minX
    maxX' = realToFrac maxX
    minY' = realToFrac minY
    maxY' = realToFrac maxY
