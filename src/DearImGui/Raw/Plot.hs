{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module: DearImGui.Raw.Plot

Main ImPlot Raw module.
-}
module DearImGui.Raw.Plot
  ( PlotContext(..)
  , createPlotContext
  , destroyPlotContext
  , getCurrentPlotContext
  , setCurrentPlotContext

  , showPlotDemoWindow

  , beginPlot
  , endPlot

  , plotLine
  , setupAxisLimits
  , setNextAxesToFit
  ) where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
import Foreign.C
import System.IO.Unsafe
  ( unsafePerformIO )

-- dear-imgui
import DearImGui
import DearImGui.Raw.Context
import DearImGui.Plot.Context
  ( implotContext )
import DearImGui.Plot.Enums
import DearImGui.Plot.Structs
import DearImGui.Raw.DrawList (DrawList(..))

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext <> implotContext)
C.include "imgui.h"
C.include "implot.h"
Cpp.using "namespace ImPlot"


-- | Wraps @ImPlotContext*@.
newtype PlotContext = PlotContext (Ptr ImPlotContext)


-- | Wraps @ImPlot::CreateContext()@.
createPlotContext :: (MonadIO m) => m PlotContext
createPlotContext = liftIO do
  PlotContext <$> [C.exp| ImPlotContext* { CreateContext() } |]

-- | Wraps @ImPlot::DestroyPlotContext()@.
destroyPlotContext :: (MonadIO m) => PlotContext -> m ()
destroyPlotContext (PlotContext contextPtr) = liftIO do
  [C.exp| void { DestroyContext($(ImPlotContext* contextPtr)); } |]

-- | Wraps @ImPlot::GetCurrentPlotContext()@.
getCurrentPlotContext :: MonadIO m => m PlotContext
getCurrentPlotContext = liftIO do
  PlotContext <$> [C.exp| ImPlotContext* { GetCurrentContext() } |]


-- | Wraps @ImPlot::SetCurrentPlotContext()@.
setCurrentPlotContext :: MonadIO m => PlotContext -> m ()
setCurrentPlotContext (PlotContext contextPtr) = liftIO do
  [C.exp| void { SetCurrentContext($(ImPlotContext* contextPtr)) } |]

-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
showPlotDemoWindow :: (MonadIO m) => m ()
showPlotDemoWindow = liftIO do
  [C.exp| void { ShowDemoWindow(); } |]

setNextAxesToFit :: MonadIO m => m ()
setNextAxesToFit = liftIO do
    [C.exp| void { SetNextAxesToFit() } |]

beginPlot :: MonadIO m => String -> m Bool
beginPlot name = liftIO do
  withCString name \namePtr ->
    (0 /=) <$> [C.exp| bool { BeginPlot($(char* namePtr)) } |]

endPlot :: MonadIO m => m ()
endPlot = liftIO do
  [C.exp| void { EndPlot(); } |]

plotLine :: MonadIO m => CString -> Ptr CFloat -> Ptr CFloat -> CInt -> m ()
plotLine label xsPtr ysPtr size = liftIO do
   [C.exp| void { PlotLine( $(char* label), $(float *xsPtr), $(float *ysPtr), $(int size) ) } |]

setupAxisLimits :: MonadIO m => ImAxis -> CDouble -> CDouble -> Maybe ImPlotCond -> m ()
setupAxisLimits (ImAxis axis) minA maxA (Just (ImPlotCond cond)) = liftIO do
  [C.exp| void { SetupAxisLimits( $(int axis), $(double minA), $(double maxA), $(int cond) ) } |]
setupAxisLimits (ImAxis axis) minA maxA Nothing = liftIO do
  [C.exp| void { SetupAxisLimits( $(int axis), $(double minA), $(double maxA) ) } |]
