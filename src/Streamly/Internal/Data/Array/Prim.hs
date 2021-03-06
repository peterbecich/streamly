{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Prim
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Prim
    (
      Array

    -- * Construction

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Stream Folds
    , fromStreamN
    , fromStream

    -- Monadic APIs
    -- , newArray
    , A.writeN      -- drop new
    , A.write       -- full buffer

    -- * Elimination

    , A.toList
    , toStream
    , toStreamRev
    , read
    , unsafeRead

    -- * Random Access
    , length
    , null
    , last
    -- , (!!)
    , readIndex
    , A.unsafeIndex
    -- , readIndices
    -- , readRanges

    -- , readFrom    -- read from a given position to the end of file
    -- , readFromRev -- read from a given position to the beginning of file
    -- , readTo      -- read from beginning up to the given position
    -- , readToRev   -- read from end to the given position in file
    -- , readFromTo
    -- , readFromThenTo

    -- , readChunksOfFrom
    -- , ...

    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksOfFrom
    -- , ...

    -- , writeIndex
    -- , writeIndices
    -- , writeRanges

    -- -- * Search
    -- , bsearch
    -- , bsearchIndex
    -- , find
    -- , findIndex
    -- , findIndices

    -- -- * In-pace mutation (for Mutable Array type)
    -- , partitionBy
    -- , shuffleBy
    -- , foldtWith
    -- , foldbWith

    -- * Immutable Transformations
--    , streamTransform

    -- * Folding Arrays
    , streamFold
    , fold

    -- * Folds with Array as the container
--    , D.lastN

    -- * Streaming array operations

    , concat
    , compact

    )
where

import Streamly.Internal.Data.Array.Prim.Types (Array(..), length)
import qualified Streamly.Internal.Data.Array.Prim.Types as A

#include "Streamly/Internal/Data/Array/PrimInclude.hs"
