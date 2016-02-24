module Ratel.Wai where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Wai
import qualified Ratel


ratelMiddleware :: Ratel.ApiKey -> Maybe Client.Manager -> Wai.Middleware
ratelMiddleware apiKey maybeManager handle request respond = do
    Exception.catch
        (do
            handle request (\ response -> do
                respond response))
        (\ exception -> do
            let err = Ratel.toError (exception :: Exception.SomeException)
            let req = Ratel.Request
                    { Ratel.requestAction = Nothing
                    , Ratel.requestCgiData = Just (Map.union
                        (Map.fromList
                            [ ("REMOTE_ADDR", show (Wai.remoteHost request))
                            , ("REQUEST_METHOD", BS.unpack (Wai.requestMethod request))
                            ])
                        (Map.fromList (map (\ (k, v) -> ("HTTP_" ++ map (\ c -> if c == '-' then '_' else Char.toUpper c) (BS.unpack (CI.foldedCase k)), BS.unpack v)) (Wai.requestHeaders request))))
                    , Ratel.requestComponent = Nothing
                    , Ratel.requestContext = Nothing
                    , Ratel.requestParams = Just (Map.fromList (map (\ (k, v) -> (BS.unpack k, maybe "" BS.unpack v)) (Wai.queryString request)))
                    , Ratel.requestSession = Nothing
                    , Ratel.requestUrl = Just (BS.unpack (Wai.rawPathInfo request) ++ BS.unpack (Wai.rawQueryString request))
                    }
            let server = Ratel.Server
                    { Ratel.serverEnvironmentName = Nothing
                    , Ratel.serverHostname = Nothing
                    , Ratel.serverProjectRoot = Nothing
                    }
            let payload = Ratel.Payload
                    { Ratel.payloadError = err
                    , Ratel.payloadNotifier = Nothing
                    , Ratel.payloadRequest = Just req
                    , Ratel.payloadServer = server
                    }
            _ <- Concurrent.forkIO (do
                _ <- Ratel.notify apiKey maybeManager payload
                return ())
            Exception.throwIO exception)
