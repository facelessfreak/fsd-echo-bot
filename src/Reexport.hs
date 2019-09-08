module Reexport ( ByteString
                , bPack
                , bUnpack
                , bReadFile
                , bsTotext

                , Text 
                , tPack
                , tUnpack 
                , textTobs
                
                , runReaderT 
                , ReaderT 
                , MonadReader
                , ask
                , asks

                , bracket
                , try
                , SomeException
                
                , Chan
                , ThreadId
                , forkIO
                , killThread
                , getChanContents
                , newChan
                , readChan
                , writeChan 

                , Async
                , async
                , wait
                , withAsync
                
                , MonadIO 
                , liftIO 
                
                , (<$>)
                , (<*>) 
                
                , IORef
                , newIORef
                , readIORef
                , writeIORef 
                
                , forever 
                , when
                
                , fromMaybe
                , fromLeft
                , fromRight ) where

import Data.ByteString.Char8 as B
import Data.Text as T
import Control.Monad.Reader ( runReaderT 
                            , ReaderT
                            , MonadReader 
                            , ask
                            , asks )
import Control.Exception ( bracket 
                         , try
                         , SomeException )

import Control.Concurrent ( Chan 
                          , ThreadId
                          , forkIO
                          , killThread
                          , getChanContents
                          , newChan
                          , readChan
                          , writeChan )
import Control.Concurrent.Async ( Async 
                                , async
                                , wait
                                , withAsync )

import Control.Monad.IO.Class ( MonadIO 
                              , liftIO )

import Control.Applicative ( (<$>) 
                           , (<*>) )

import Data.IORef ( IORef
                  , newIORef 
                  , readIORef
                  , writeIORef )

import Control.Monad ( forever 
                     , when )

import Data.Maybe ( fromMaybe )
import Data.Either ( fromLeft 
                   , fromRight )

 
tPack   = T.pack
tUnpack = T.unpack

bPack   = B.pack
bUnpack = B.unpack
bReadFile = B.readFile

bsTotext = tPack . bUnpack
textTobs = bPack . tUnpack
