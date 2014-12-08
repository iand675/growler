# Growler

Growler provides a very similar interface to scotty, with slight tweaks for performance and a few feature tradeoffs. Growler provides the ability to abort actions (handlers) with arbitrary responses, not just in the event of redirects or raising errors. Growler avoids coercing everything into lazy Text values and reading the whole request body into memory. It also eliminates the ability to abort the handler and have another handler handle the request instead (Scotty's next function).

API is still in flux, so use at your own risk. Pull requests / issues are welcome.

## Examples

### Hello World
``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Growler
import Data.Monoid ((<>))

main = growl id defaultConfig $ do
  get "/" $ text "Hello, World!"
  get "/:name" $ do
    name <- param "name"
    text ("Hello, " <> name <> "!")
```
