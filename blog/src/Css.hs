module Css where

import Lucid.Css (Css)

myCss :: Css
myCss = css_ $
    "body {" <>
    "  font-family: Arial, sans-serif;" <>
    "  background-color: #f0f0f0;" <>
    "}" <>
    ".container {" <>
    "  width: 80%;" <>
    "  margin: 0 auto;" <>
    "}"