import Mitchell.Prelude

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XDeriveAnyClass"
  , "-XDeriveGeneric"
  , "-XFlexibleContexts"
  , "-XLambdaCase"
  , "-XNoImplicitPrelude"
  , "-XOverloadedStrings"
  , "-XPatternSynonyms"
  , "-XRecordWildCards"
  , "-XScopedTypeVariables"
  , "-XTupleSections"
  , "-XTypeFamilies"
  , "src"
  ]
