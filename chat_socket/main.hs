import Network.Socket
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
    --topic <- liftM init $ recv sock 100
    let topic = "100"
    case Map.lookup topic channelMap of
        Nothing ->
            --send sock "Topic " ++ show topic ++ " doesn't exist"
            runConn (sock, sockadr) channelMap
            --return ()

        (Just channel) ->
            bla sock channel


bla :: Socket -> Chan Msg -> IO()
bla sock chan = do
    let broadcast msg = writeChan chan msg
    send sock "Name eingeben \n"
    user <- recv sock 100
    chan' <- dupChan chan
    forkIO $ forever $ do
        line <- readChan chan'
        if (user /= (fst line)) then
            send sock $ (fst line) ++ ": " ++ (snd line) ++ "\n"
        else
            return 0

    forever $ do
        line <- liftM init (recv sock 100)
        broadcast $ (user, line)

