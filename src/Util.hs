module Util where

-- | Strip version string from given package name.
--
-- The package name @main@ is returned verbatim.  If the packga name is not
-- @main@, we assume that there is always a version string, delimited with a
-- @\'-\'@ from the package name.  Behavior is unspecified for package names
-- that are neither @main@ nor have a version string.
--
-- Examples:
--
-- >>> stripVersion "main"
-- "main"
--
-- >>> stripVersion "foo-0.0.0"
-- "foo"
--
-- >>> stripVersion "foo-bar-0.0.0"
-- "foo-bar"
stripVersion :: String -> String
stripVersion p = case p of
  "main" -> p
  _ -> reverse $ tail $ dropWhile (/= '-') $ reverse p
