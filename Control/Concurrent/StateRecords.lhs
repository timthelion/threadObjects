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

>module StateRecords where

>import ThreadObject

>data RecorderSignal signal = RecorderSignal Bool (Maybe signal) 

>type StateRecords a signal = ThreadObject ((Maybe a, [a],[a]),(ThreadObject a signal)) ()

>stateRecords :: ThreadObject a signal -> IO (StateRecords a signal)
>stateRecords to = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (InitializedNotSynced ((Nothing,[],[]),to)) noSyncOnGet noSyncOnPut
> return stateRecordsObject 

>recordState ::  Int -> StateRecords a signal -> a -> IO ()
>recordState n stateRecordsObject value = do
>  updateIO stateRecordsObject 
>    (\((lastStateMaybe,stack1,stack2),to)-> do

     print "length"
     print $ length stack1
     print "n"
     print n

>     if length stack1 >= n
>     then case lastStateMaybe of
>       Just lastState -> do

         print 1

>         return ((Just value,lastState:[],stack1), to)
>       Nothing   -> do

         print 2

>         return ((Just value,[],stack1), to)
>     else case lastStateMaybe of
>       Just lastState -> do

        print 3;

>        return ((Just value,lastState:stack1,stack2), to)
>       Nothing   -> do 

        print 4;

>        return ((Just value,stack1,stack2), to))

| This is to undo an action applied the thread object which is used as your "metronome", that thread object, within who's syncOnPut, are the recordState commands.  

>undoStateActionOfRecorder :: StateRecords a (RecorderSignal signal) -> IO Bool
>undoStateActionOfRecorder stateRecordsObject = do
> undoStateAction' stateRecordsObject (\to value -> updateWithSignal to (\_->value) (RecorderSignal False Nothing))


>undoStateAction :: StateRecords a signal -> IO Bool
>undoStateAction stateRecordsObject = do
>  undoStateAction' stateRecordsObject (\to value -> update to (\_->value))

>undoStateAction' :: StateRecords a signal -> (ThreadObject a signal -> a -> IO ()) -> IO Bool
>undoStateAction' stateRecordsObject myUpdate = do
>  updateIOReturning stateRecordsObject
>    (\(stacks,to)-> do
>     case stacks of
>       (Just _,value:stack1,stack2) -> do
>           myUpdate to value

           print "Jv"

>           return (((Nothing,stack1,stack2),to),True)

>       (Nothing,value:stack1,stack2) -> do
>           myUpdate to value

           print "Nv"

>           return (((Nothing,stack1,stack2),to),True)

>       (Just _,[],value:stack2) -> do
>           myUpdate to value

           print "J[]"

>           return (((Nothing,stack2,[]),to),True)

>       (Nothing,[],value:stack2) -> do
>           myUpdate to value

           print "N[]"

>           return (((Nothing,stack2,[]),to),True)

>       (Just _,[],[])  -> do

          print "J[]"

>          return (((Nothing,[],[]),to),False)

>       (Nothing,[],[])  -> do

          print "N[]"

>          return (((Nothing,[],[]),to),False))
