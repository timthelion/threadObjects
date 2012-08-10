GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

>module Control.Concurrent.ThreadObject where
>import Control.Concurrent
>import Control.Exception
>import Prelude hiding (catch)
>import System.IO

This is a simple library for creating mutable objects in Haskell.  

To create an object you need to run

myObject <- threadObject
objectInit
 myObject
 myObjectsInitialValue -- For example: (SyncWithoutSignal (SeedInitially someInitialValue))
                       -- or (SyncWithoutSignal (SyncOnGet someSyncOnGetFunction someInitiallValue))
 syncorBindFunction
 exceptionHandler -- For example: (handleExceptionStdErr "nameOfMyThreadObject")

The sync function is run every time the object's value is updated.  This makes it easy to "bind" an object to a GTK widget which displays it(for example).

The nice thing about this library is that the two most frequently used functions are 100% thread safe(so long as you are using non mutable values and not GTK widgets or some other weirdness)!

If you want to get an object's value, you can use getObjectValue.  This is thread safe.

If you want to get an objects value, do something to it, and then set the object's value to a new value, you can use update.

update takes two arguments.  The object, and a non IO typed haskell function which updates the value.  This is also thread safe.

The bind/sync function, and the updateIO functions are NOT thread safe, or even thread aware, however.  Be carefull of these.

>data ThreadObject a signal = ThreadObject{
>     tickerMVar     :: MVar (ActionType a signal)}

>data ActionType a signal =
>      IOAction (a -> IO a) (Maybe signal) |
>      PureAction (a -> a) (Maybe signal)  |
>      SetSyncOnGet (a -> IO a)            |
>      SetSyncOnPut (a -> (Maybe signal) -> IO ()) |
>      GetObjectValueIO (a -> IO ())       |
>      FreeObject (MVar Bool)

>threadObject :: IO (ThreadObject a signal)

>threadObject = do
>    myTickerMVar    <- newEmptyMVar
>    return (ThreadObject myTickerMVar)

Do we want to seed our thread objects state with an initial value?  Or do we want to SyncOnGet, for example to synchronize our objects value with user inputed text?

>data SeedOrSyncOnGet a =
>      SeedInitially a |
>      SyncOnGet (a -> IO a) a

Shoud we run SyncOnPut imidiately when the object is initialized with a new seed value?  Should we send a signal to SyncOnPut if we do so?

>data InitialThreadState a signal =
> DoNotSync (SeedOrSyncOnGet a) |
> SyncWithoutSignal (SeedOrSyncOnGet a) |
> SyncWithSignal (SeedOrSyncOnGet a) (Maybe signal)

| This is one of our big important functions.  In order to create a thread object you must first make a reference to it, with the "threadObject" function, then you have to start the object's life.

Here you pass:

1. The reference you created with "threadObject".
2. A sync function, to be run each time the object is updated.
3. A function to be run if an exception arises while an object is being updated. This function takes the value of the thread object before the update began to run, and returns a new value for the thread object.  Mainly, you'll want to use this opportunity to display a dialog telling the user that something whent wrong, perhaps save to a special temp file, and return the same value you recieved(you might return a different value if you determine the previous value of the thread object to be somehow corrupt).  A sample exception handler is provided: handleExceptionStdErr

What are these signals anyways?  They are of very little importance actually.  They are little extra messages sent by an update command to the sync command.  An example of this, is when we update because we are "Undoing" an action, and our sync command happens to have an action recorder that is creating our undo cue, we have to have a way for that update command to tell the sync command *not* to record that update.

>objectInit :: Exception exception => ThreadObject a signal -> InitialThreadState a signal -> (a -> (Maybe signal) -> IO ()) -> (a -> exception -> IO a) -> IO ()
>objectInit to (DoNotSync seedOrSyncOnGet) syncOnPut exceptionHandler = do
>    (value,syncOnGet) <- case seedOrSyncOnGet of
>     SeedInitially seed -> return (seed,noSyncOnGet)
>     SyncOnGet syncOnGet' seed -> do
>      value' <- catch (syncOnGet' seed) (exceptionHandler seed)
>      return (value',syncOnGet')
>    _ <- forkIO $ loopObject to syncOnGet syncOnPut value exceptionHandler
>    return ()

>objectInit to (SyncWithSignal seedOrSyncOnGet signal) syncOnPut exceptionHandler = do
>    (value,syncOnGet) <- case seedOrSyncOnGet of
>     SeedInitially seed -> return (seed,noSyncOnGet)
>     SyncOnGet syncOnGet' seed -> do
>      value' <- catch (syncOnGet' seed) (exceptionHandler seed)
>      return (value',syncOnGet')
>    _ <- forkIO $ loopObjectSyncOnPutPhase to syncOnGet syncOnPut value value signal exceptionHandler
>    return ()

>objectInit to (SyncWithoutSignal seedOrSyncOnGet) syncOnPut exceptionHandler = do
>    (value,syncOnGet) <- case seedOrSyncOnGet of
>     SeedInitially seed -> return (seed,noSyncOnGet)
>     SyncOnGet syncOnGet' seed -> do
>      value' <- catch (syncOnGet' seed) (exceptionHandler seed)
>      return (value',syncOnGet')
>    _ <- forkIO $ loopObjectSyncOnPutPhase to syncOnGet syncOnPut value value Nothing exceptionHandler
>    return ()

>loopObjectSyncOnPutPhase :: Exception exception => ThreadObject a signal -> (a -> IO a) -> (a -> (Maybe signal) -> IO ()) -> a -> a -> (Maybe signal) -> (a -> exception -> IO a) ->  IO ()
>loopObjectSyncOnPutPhase to syncOnGet syncOnPut value oldValue signal exceptionHandler = do
> repairedValueMaybe <- catch ((syncOnPut value signal) >> return Nothing) (\exception -> exceptionHandler oldValue exception >>= (\handlerValue -> return $ Just handlerValue))
> case repairedValueMaybe of
>  Just repairedValue -> 
>   loopObject to syncOnGet syncOnPut repairedValue exceptionHandler
>  Nothing ->
>   loopObject to syncOnGet syncOnPut value exceptionHandler


>loopObject :: Exception exception => ThreadObject a signal -> (a -> IO a) -> (a -> (Maybe signal) -> IO ()) -> a -> (a -> exception -> IO a) ->  IO ()
>loopObject  to@(ThreadObject myTickerMVar) syncOnGet syncOnPut value exceptionHandler = do
> actionType <- takeMVar myTickerMVar
> case actionType of
>  PureAction action mySignal -> do
>   value'  <- catch (syncOnGet value) (exceptionHandler value)
>   value'' <- catch (return $ action value') (exceptionHandler value') 
>   loopObjectSyncOnPutPhase to syncOnGet syncOnPut value'' value mySignal exceptionHandler
>  IOAction   action mySignal -> do
>   value'  <- catch (syncOnGet value) (exceptionHandler value)
>   value'' <- catch (action value') (exceptionHandler value)
>   loopObjectSyncOnPutPhase to syncOnGet syncOnPut value'' value mySignal exceptionHandler
>  GetObjectValueIO getter  -> do
>   value'  <- catch (syncOnGet value) (exceptionHandler value)
>   getter value'
>   loopObject to syncOnGet syncOnPut value' exceptionHandler
>  SetSyncOnGet syncOnGet'  -> do
>   loopObject to syncOnGet' syncOnPut value exceptionHandler
>  SetSyncOnPut syncOnPut'  -> do
>   loopObjectSyncOnPutPhase to syncOnGet syncOnPut' value value Nothing exceptionHandler
>  FreeObject freedMVar -> putMVar freedMVar True

>update :: ThreadObject a signal -> (a -> a) -> IO ()
>update to action = do
>    updateWithSignal' to action Nothing

>updateBlock :: ThreadObject a signal -> (a -> a) -> IO ()
>updateBlock to action = do
>    updateReturning to (\value -> (action value,()))

>updateWithSignal :: ThreadObject a signal -> (a -> a) -> signal -> IO ()
>updateWithSignal to action signal = do
>    updateWithSignal' to action (Just signal)

>updateWithSignal' :: ThreadObject a signal -> (a -> a) -> (Maybe signal) -> IO ()
>updateWithSignal' (ThreadObject myTickerMVar) action signal = do
>    putMVar myTickerMVar (PureAction action signal)

>updateReturning :: ThreadObject a signal -> (a -> (a,b)) -> IO b
>updateReturning to action = do
>  returnValueMVar <- newEmptyMVar
>  updateIO to (\value -> do
>   (value',returnValue) <- return (action value)
>   putMVar returnValueMVar returnValue
>   return value')
>  takeMVar returnValueMVar

>updateIOReturning :: ThreadObject a signal -> (a -> IO (a,b)) -> IO b
>updateIOReturning to action = do
>  returnValueMVar <- newEmptyMVar
>  updateIO to (\value -> do
>   (value',returnValue) <- action value
>   putMVar returnValueMVar returnValue
>   return value')
>  takeMVar returnValueMVar

| updateIOReturningInThisThread is a very special function for a very special case.  Normally, updateIOReturning blocks the calling thread, and then preforms the given IO actions in the ThreadObject's thread.  However, if the calling thread is the same thread as GTK is running in, we end up with a problem.  We cannot make any GTK calls from within a standard updateIOReturning.  We cannot call them directly, because we are no longer in GTK's thread, and we cannot postGUI--- because GTK's thread is blocked!  So we make this function, without any guarantees to it's preformance, just for you.

I recomend you don't look at it's source code, it's butt ugly :D

>updateIOReturningInThisThread :: ThreadObject a signal -> (a -> IO (a,b)) -> IO b
>updateIOReturningInThisThread to action = do
>  valueMVar <- newEmptyMVar
>  valueMVar' <- newEmptyMVar
>  updateIONoBlock to (\value -> do
>   putMVar valueMVar value
>   takeMVar valueMVar')
>  value <- takeMVar valueMVar
>  (value',returnValue) <- action value
>  putMVar valueMVar' value'
>  return returnValue


f :: (a -> IO (a,b)) -> IO b
f action = do
 (a',b) <- action 1
 print a'
 return b

f $ f $ f (\a b c->return (a,(b,c)))

mainthread:
updateMulti to1 $ alsoUpdate to2 $ finallyUpdate to3 (\c b a -> (c,(b,a)))
to1:
value' <- action value -- action :: t1 -> IO t1
to2:                                      ^ putMVar v1MVar
value' <-action value  -- action :: t2 -> IO t2
to3:                                      ^ putMVar v2MVar
value' <-action value  -- action :: t3 -> IO t3

action v1 v2 v3 = (v3,(v2,(v1)))

>updateMulti :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>updateMulti to action = do
>  updateIONoBlock to action

>updateMultiWithSignal :: ThreadObject a signalA -> signalA -> (a -> IO a) -> IO ()
>updateMultiWithSignal to signal action = do
>  updateIONoBlockWithSignal to action signal

>alsoUpdate :: ThreadObject a signal -> (t -> a -> IO (a, b)) -> t -> IO b
>alsoUpdate to action = 
> (\a -> do
>  updateIOReturning to $ action a)

>finallyUpdate :: ThreadObject a signal -> (t -> a -> (a, b)) -> t -> IO b
>finallyUpdate to action = do
> (\a -> do
>  updateReturning to $ action a)

>updateHelper :: a -> b -> MVar a -> (a -> b -> IO (a,b)) -> IO b
>updateHelper a b aMVar action = do
>    (a',b') <- action a b
>    putMVar aMVar a'
>    return b'

>updateWith :: ThreadObject a signalA -> ThreadObject b signalB -> (a -> b -> b) -> IO ()
>updateWith to1 to2 action = do
> value1 <- getObjectValue to1
> update to2 (action value1)

>updateWith2 :: ThreadObject a signalA -> ThreadObject b signalB -> ThreadObject c signalC -> (a -> b -> c -> c) -> IO ()
>updateWith2 to1 to2 to3 action = do
> value1 <- getObjectValue to1
> value2 <- getObjectValue to2
> update to3 (action value1 value2)

| updateIO blocks by default.  Use updateIONoBlock if you don't want this to be the case.

>updateIO :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>updateIO (ThreadObject myTickerMVar) action = do
>    lock <- newEmptyMVar
>    putMVar myTickerMVar $ IOAction
>        (\x -> do 
>                x' <- action x;
>                putMVar lock Nothing;
>                return x') Nothing
>    _ <- takeMVar lock
>    return ()

>updateIONoBlock :: ThreadObject a singalA -> (a -> IO a) -> IO ()
>updateIONoBlock to action = do
>    updateIONoBlockWithSignal' to action Nothing

>updateIONoBlockWithSignal :: ThreadObject a signalA -> (a -> IO a) -> signalA -> IO ()
>updateIONoBlockWithSignal to action signal = do
>    updateIONoBlockWithSignal' to action (Just signal)

>updateIONoBlockWithSignal' :: ThreadObject a signalA -> (a -> IO a) -> Maybe signalA -> IO ()
>updateIONoBlockWithSignal' (ThreadObject myTickerMVar) action signal = do
>    putMVar myTickerMVar (IOAction action signal) 


>getObjectValue :: ThreadObject a signalA -> IO a
>getObjectValue (ThreadObject myTickerMVar) = do
>    valueMVar <- newEmptyMVar
>    putMVar myTickerMVar (GetObjectValueIO (\value -> putMVar valueMVar value))
>    takeMVar valueMVar

>setSyncOnGet :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>setSyncOnGet (ThreadObject myTickerMVar) syncOnGet = do
>    putMVar myTickerMVar $ SetSyncOnGet syncOnGet

>setSyncOnPut :: ThreadObject a signalA -> (a -> (Maybe signalA) -> IO ()) -> IO ()
>setSyncOnPut (ThreadObject myTickerMVar) syncOnPut = do
>    putMVar myTickerMVar $ SetSyncOnPut syncOnPut

>noSyncOnGet :: a -> IO a
>noSyncOnGet value = return value

>noSyncOnPut :: a -> (Maybe signalA) -> IO ()
>noSyncOnPut _ _ = return ()

handleExceptionStdErr :: String -> a -> exception -> IO a

>handleExceptionStdErr threadObjectName oldValue exception =
> let typedException::SomeException
>     typedException = exception::SomeException
> in do
> hPutStrLn stderr $ "Exception caught while updating ThreadObject: "++threadObjectName
> hPutStrLn stderr (show typedException)
> return oldValue

|Returns once all opperations on the object are finished and the object is freed.

>freeObject :: ThreadObject a signalA -> IO ()
>freeObject (ThreadObject myTickerMVar) = do
>  freedMVar <- newEmptyMVar
>  putMVar myTickerMVar (FreeObject freedMVar)
>  _ <- takeMVar freedMVar
>  return ()

Some people say that "without mutability, there is very little that can go wrong" others say that "without mutability, there is very little that can go."

still others ask if time exists, and if so called mutability is anything more than a less coherent whole.
