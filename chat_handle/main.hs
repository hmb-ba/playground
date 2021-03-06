import Network.Socket 
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix) 

import Data.Map

type User = String
type Msg = (User, String)
type ChannelMap = Map String (Chan Msg)

main :: IO()
main = do
	--create server 
	sock <- socket AF_INET Stream defaultProtocol 
	--make socket immediately reusable
	setSocketOption sock ReuseAddr 1
	--listen on TCP port 4242
	bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
	--allow a maximum of 2 outstanding connections
	listen sock 5
	channelA <- newChan
	channelB <- newChan
	mainLoop sock $ fromList [("1", channelA), ("2", channelB)]

mainLoop :: Socket -> ChannelMap -> IO()
mainLoop sock chan = do 
	--accept one connection and handle it 
	conn <- accept sock
	forkIO (runConn conn chan) 
	mainLoop sock chan

runConn :: (Socket, SockAddr) -> ChannelMap -> IO()
runConn (sock, addr) chanMap = do 
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering
	findChannel hdl chanMap	

findChannel:: Handle -> ChannelMap -> IO()
findChannel hdl chanMap = do
	hPutStrLn hdl "Bitte geben Sie die Id des Chatrooms ein:"
	topic <- liftM init $ hGetLine hdl 

	case Data.Map.lookup topic chanMap of
	      Nothing -> 
			do
			hPutStrLn hdl "Topic ungueltig!"
			findChannel hdl chanMap
	      (Just channel) ->
			chat hdl channel 

chat :: Handle -> Chan Msg -> IO()
chat hdl chan = do
	chan' <- dupChan chan
	let broadcast msg = writeChan chan' msg
	
	hPutStrLn hdl "Bitte geben sie Ihr Username ein:" 
	usr <- liftM init $ hGetLine hdl
	broadcast (usr, " logged in")
	
	--fork off thread for reading from the duplicated channel
	forkIO $ forever $ do
		msg <- readChan chan'
		when ((fst msg) /= usr) $
			hPutStrLn hdl $ fst (msg) ++ ":" ++ snd (msg)
	
	--read lines from socket and echo them back to the user 
	forever $ do
		line <- liftM init $ hGetLine hdl
		broadcast (usr, line)

