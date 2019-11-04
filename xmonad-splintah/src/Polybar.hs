{-# LANGUAGE NamedFieldPuns #-}

module Polybar where

import           XMonad.Hooks.DynamicLog

-- | 'defPolybarPP' @file@ is the default 'PP' configuration for Polybar,
-- which writes to @file@.
--
-- The Polybar module would look something like this, where @<file>@ is the same
-- file as supplied to 'defPolybarPP':
-- > [module/xmonad]
-- > type = custom/script
-- > exec = tail -F <file>
-- > exec-if = [ -p <file> ]
-- > tail = true
defPolybarPP :: String -> PP
defPolybarPP file = def
  { ppOutput = appendFile file . (<> "\n")
  }

-- | 'color' @fg@ @bg@ @s@ formats @s@ with foreground colour @fg@ and
-- background colour @bg@.
color :: String -> String -> String -> String
color fg bg = foreground fg . background bg

-- | 'foreground' @fg@ @s@ formats @s@ with foreground colour @fg@.
foreground :: String -> String -> String
foreground c s = "%{F" <> c <> "}" <> s <> "%{F-}"

-- | 'background' @bg@ @s@ formats @s@ with background colour @bg@.
background :: String -> String -> String
background c s = "%{B" <> c <> "}" <> s <> "%{B-}"

underline :: String -> String -> String
underline c s = "%{u" <> c <> "}" <> s <> "%{-u}"

overline :: String -> String -> String
overline c s = "%{o" <> c <> "}" <> s <> "%{-o}"

font :: Int -> String -> String
font f s = "%{T" <> show f <> "}" <> s <> "%{T-}"

offset :: Int -> String -> String
offset o s = "%{O" <> show o <> "}" <> s

action :: Int -> String -> String -> String
action buttonIndex command s =
  "%{A" <> show buttonIndex <> ":" <> concatMap escapeColon command <> ":}" <> s <> "%{A}"
  where
    escapeColon ':' = "\\:"
    escapeColon c   = [c]
