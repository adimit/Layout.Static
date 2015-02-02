{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Static
-- Copyright   :  (c) Aleksandar Dimitrov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <aleks.dimitrov@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A static layout inspired by ion3, and notion.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Static (
  -- * Usage
  -- $usage
  Static(..)
) where

-- $usage
-- Don't use it yet

import XMonad
import XMonad.StackSet
import XMonad.Layout.Tabbed

data Split = Split deriving (Show, Read)
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Read)

data Static a = Static { layoutTree :: Tree Split } deriving (Read, Show)

instance LayoutClass Static a where
  description _ = "Static"
  doLayout (Static t) r = undefined
  handleMessage (Static t) m = undefined

