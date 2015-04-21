{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module MVarDom where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.MVar
import Data.Tree
import Data.Text


data Tag = Div_ | Span_ | CData_ | Custom_ Text deriving Show

data MVarDOMAttr = MVA (Text, Text) deriving Show
data MVarDOM = MVD Tag (MVar [MVarDOMAttr]) [MVar MVarDOM]

type DOM = MVar MVarDOM

data PureDOM = PureDOM (Tag, [MVarDOMAttr]) [PureDOM] deriving Show

printDOM :: DOM -> IO ()
printDOM dom = do
  MVD t (mattrs) cs <- takeMVar dom
  attrs <- takeMVar mattrs
  print (t,attrs)
  mapM_ (\c -> putStr "  " >> printDOM c) cs

pure2mvar :: MonadIO m => PureDOM -> m DOM
pure2mvar (PureDOM (tag,attrs) children) = do
  mcs <- mapM pure2mvar children
  mattrs <- liftIO $ newMVar attrs
  liftIO $ newMVar (MVD tag mattrs mcs)
  

think
  :: IO
       (MVar
          (String,
           [(String, String)],
           [MVar (String, [(String, String)], [MVar (String, String)])]))
think = do
  msg1 <- newMVar ("cdata", "thing1")
  msg2 <- newMVar ("cdata", "thing2")
  thing1 <- newMVar ("span", [("id","1")], [msg1])
  thing2 <- newMVar ("span", [("id","2")], [msg2])
  let children = [thing1, thing2]
  root <- newEmptyMVar
  putMVar root ("div", [("id","0")], children)
  return root

-- ex1 :: MonadIO m => m DOM
ex1 :: IO DOM
ex1 = do
  attrs <- newMVar []
  newMVar (MVD Div_  attrs [])


pureEx :: PureDOM
pureEx =
  PureDOM (Div_, [MVA ("id","1")]) [PureDOM (Div_, [MVA ("id","2")]) []]

mex :: MonadIO m => m DOM
mex = liftIO $ pure2mvar pureEx  

