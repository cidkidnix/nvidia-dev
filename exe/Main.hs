{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Posix.Files
import System.Posix.Types
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import System.Directory
import Control.Monad
import Data.Bits

-- nvidiactl minor is always 255 ???
-- nvidia-uvm-tools is (minor + 1)_
-- nvidia-uvm is just minor
-- I don't think we have to create caps, but we would have to at least make 1 and 2


main :: IO ()
main = do
    procDevices <- getDevices
    let wantedDevices = [ "nvidia-modeset", "nvidia", "nvidiactl", "nvidia-uvm", "nvidia-frontend", "nvidia-caps", "nvidia-nvlink" ]
    let devices = flip concatMap wantedDevices $ \d -> do
          let device = concatMap (T.splitOn " ") $ filter (\x -> d `T.isSuffixOf` x) procDevices
          case device of
            [] -> []
            [char, procModule] -> do
                let modFunc = textToNvidiaDevices procModule
                case modFunc of
                  Nothing -> []
                  Just func -> [func char]
            _ -> []
    gpus <- getNvidiaGPUs
    gpuData <- getGpuData gpus
    let parsedGpuData = parseGpuData gpuData
    let gpuDeviceNumbers = map (NvidiaDeviceId . either (error "No GPUs found!") (\(x,_) -> x) . TR.decimal . last . T.splitOn "\t ") parsedGpuData

    putStrLn $ "GPU PCI Ids: " <> show gpus
    putStrLn $ "GPU Minor Numbers: " <> show gpuDeviceNumbers

    createDevices gpuDeviceNumbers devices

    pure ()


getDevices :: IO [Text]
getDevices = (T.splitOn "\n" . T.pack) <$> readFile "/proc/devices"

getNvidiaGPUs :: IO [PCIDevice]
getNvidiaGPUs = (map (PCIDevice . T.pack)) <$> listDirectory "/proc/driver/nvidia/gpus"

getGpuData :: [PCIDevice] -> IO [[Text]]
getGpuData gpus = flip mapM gpus $ \(PCIDevice x) -> do
    gpuData <- T.pack <$> (readFile $ "/proc/driver/nvidia/gpus/" <> T.unpack x <> "/information")
    let gpuInfo = T.splitOn "\n" gpuData
    pure gpuInfo


parseGpuData :: [[Text]] -> [Text]
parseGpuData gpuData = flip concatMap gpuData $ \x -> filter (\c -> "Device Minor" `T.isPrefixOf` c) x

createDevices :: [NvidiaDeviceId] -> [NvidiaDevices] -> IO ()
createDevices devIds devices = void $ flip mapM devices $ \case
    NvidiaModeset _ -> pure ()
    NvidiaCtl x -> do
        putStrLn "NVIDIACTL"
        exists <- fileExist "/dev/nvidiactl"
        unless exists $ makeNod (textToInt x) 255 "/dev/nvidiactl"
    NvidiaUvm x -> void $ flip mapM devIds $ \deviceId -> do
        putStrLn "NVIDIA-UVM"
        uvmExists <- fileExist "/dev/nvidia-uvm"
        uvmToolsExists <- fileExist "/dev/nvidia-uvm-tools"

        unless (uvmExists || uvmToolsExists) $ do
          makeNod (textToInt x) (unNvidiaDeviceId deviceId) "/dev/nvidia-uvm"
          makeNod (textToInt x) ((unNvidiaDeviceId deviceId) + 1) "/dev/nvidia-uvm-tools"
    Nvidia x -> void $ flip mapM devIds $ \deviceId -> do
        let devId = unNvidiaDeviceId deviceId
        putStrLn $ "Nvidia" <> show devId
        exists <- fileExist $ "/dev/nvidia" <> show devId
        unless exists $ makeNod (textToInt x) devId ("/dev/nvidia" <> show devId)
    _ -> pure ()

data NvidiaDevices
  = NvidiaModeset Text
  | Nvidia Text
  | NvidiaCtl Text
  | NvidiaUvm Text
  | NvidiaFrontend Text
  | NvidiaCaps Text
  | NvidiaNVLink Text
  deriving (Show, Eq, Ord)

newtype NvidiaDeviceId = NvidiaDeviceId { unNvidiaDeviceId :: Integer }
  deriving (Show, Eq, Ord)

newtype PCIDevice = PCIDevice { unPCIDevice :: Text }
  deriving (Show, Eq, Ord)

toNvidiaDeviceText :: NvidiaDevices -> Text
toNvidiaDeviceText = \case
  NvidiaModeset _ -> "nvidia-modeset"
  Nvidia _ -> "nvidia"
  NvidiaCtl _ -> "nvidiactl"
  NvidiaUvm _ -> "nvidia-uvm"
  NvidiaFrontend _ -> "nvidia-frontend"
  NvidiaCaps _ -> "nvidia-caps"
  NvidiaNVLink _ -> "nvidia-nvlink"

toNvidiaDeviceNode :: NvidiaDevices -> Maybe FilePath
toNvidiaDeviceNode = \case
  NvidiaModeset _ -> Just "/dev/nvidia-modeset"
  Nvidia _ -> Nothing
  NvidiaCtl _ -> Just "/dev/nvidiactl"
  NvidiaUvm _ -> Just "/dev/nvidia-uvm"
  NvidiaFrontend _ -> Nothing
  NvidiaCaps _ -> Nothing
  NvidiaNVLink _ -> Nothing

textToNvidiaDevices :: Text -> Maybe (Text -> NvidiaDevices)
textToNvidiaDevices = \case
    "nvidia-modeset" -> Just NvidiaModeset
    "nvidia" -> Just Nvidia
    "nvidiactl" -> Just NvidiaCtl
    "nvidia-uvm" -> Just NvidiaUvm
    "nvidia-frontend" -> Just NvidiaFrontend
    "nvidia-caps" -> Just NvidiaCaps
    "nvidia-nvlink" -> Just NvidiaNVLink
    _ -> Nothing


type Mode = Integer
type Major = Integer
type Minor = Integer


makeNod :: Major -> Minor -> FilePath -> IO ()
makeNod maj min' fp = do
    createDevice fp characterSpecialMode (mkDev maj min')
    setMode fp

mkDev :: Major -> Minor -> DeviceID
mkDev maj min' = fromIntegral $ ((maj .<<. minorbits) .|. min')
  where
      minorbits = 8


-- This might be dangerous, look into
-- Also do we care about the old special "char" mode,
-- Seems like we shouldn't and the kernel (or this library) prevents from overwriting
setMode :: FilePath -> IO ()
setMode fp = setFileMode fp mode
  where
    mode = ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. groupWriteMode .|. otherReadMode .|. otherWriteMode


textToInt :: Text -> Integer
textToInt a = case TR.decimal a of
                Left _ -> error "Couldn't parse"
                Right (i, _) -> i
