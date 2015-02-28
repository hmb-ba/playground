import Network.Socket
import Network.Socket.SendFile
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import qualified Data.Map as Map

type ChannelMap = Map.Map String (Chan Msg)
type User = String
type Msg = (User, String)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2

    a <- newChan
    b <- newChan
    c <- newChan

    let channels = Map.fromList[("100", a),("101", b),("102", c)]

    mainLoop sock channels


mainLoop :: Socket -> ChannelMap -> IO ()
mainLoop sock channelMap = do
    conn <- accept sock
    forkIO (runConn conn channelMap) -- creates a new thread
    mainLoop sock channelMap

runConn :: (Socket, SockAddr) -> ChannelMap -> IO ()
runConn (sock, sockadr) channelMap = do
    send sock "Topic nr. eingeben \n"
    t <- recv sock 1024
    let topic = removeNewLines t
    case Map.lookup topic channelMap of
        Nothing ->
            --send sock "Topic " ++ show topic ++ " doesn't exist"
            runConn (sock, sockadr) channelMap
        (Just channel) ->
            chat sock channel


chat :: Socket -> Chan Msg -> IO()
chat sock chan = do
    let broadcast msg = writeChan chan msg
    send sock "Name eingeben \n"
    user <- recv sock 1024
    chan' <- dupChan chan
    send sock "History: \n"
    sendFile sock "/tmp/chat"
    -- read message (read from channel and send to client socket)
    forkIO $ forever $ do
        line <- readChan chan'
        if (user /= (fst line)) then
            send sock $ (removeNewLines $ fst line) ++ ": " ++ (snd line) ++ "\n"
        else
            return 0
    -- write message (to channel)
    forever $ do
        line <- (recv sock 1024)
        broadcast $ (user, removeNewLines line)
        appendFile "/tmp/chat" $ (removeNewLines user) ++ ": " ++ line ++ "\n"


removeNewLines :: String -> String
removeNewLines s = filter (/= '\n') $ filter (/= '\r') s
