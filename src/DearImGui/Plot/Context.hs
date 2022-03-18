{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}

module DearImGui.Plot.Context where

-- containers
import qualified Data.Map.Strict as Map

-- inline-c
import Language.C.Inline.Context
  ( Context(..) )
import Language.C.Types
  ( pattern TypeName )

-- dear-implot
import DearImGui.Plot.Structs

-- dear-imgui-generator -> implot
import DearImGui.Plot.Generator
  ( enumerationsTypesTable )

--------------------------------------------------------------------------------

implotContext :: Context
implotContext = mempty
  { ctxTypesTable = enumerationsTypesTable <>
    Map.fromList
      [ ( TypeName "ImPlotContext", [t| ImPlotContext |] )
      ]
  }
