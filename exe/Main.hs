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
        modelData = getModelData gpuData
        gpuDeviceNumbers = map (NvidiaDeviceId . either (error "No GPUs found!") (\(x,_) -> x) . TR.decimal . last . T.splitOn "\t ") parsedGpuData
        gpuModels = map (last . T.splitOn "\t ") modelData


    flip mapM_ gpus $ \x -> do
        putStrLn $ T.unpack $ "GPU Found: " <> (unPCIDevice x)

    flip mapM_ gpuModels $ \x -> do
        putStrLn $ T.unpack $ "GPU Model: " <> x

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

getModelData :: [[Text]] -> [Text]
getModelData gpuData = flip concatMap gpuData $ \x -> filter (\c -> "Model" `T.isPrefixOf` c) x

createDevices :: [NvidiaDeviceId] -> [NvidiaDevices] -> IO ()
createDevices devIds devices = void $ flip mapM devices $ \case
    NvidiaCtl x -> do
        putStrLn "/dev/nvidiactl"
        exists <- fileExist "/dev/nvidiactl"
        unless exists $ makeChar (textToInt x) 255 "/dev/nvidiactl"
    NvidiaUvm x -> void $ flip mapM devIds $ \deviceId -> do
        putStrLn "/dev/nvidia-uvm and /dev/nvidia-uvm-tools"
        uvmExists <- fileExist "/dev/nvidia-uvm"
        uvmToolsExists <- fileExist "/dev/nvidia-uvm-tools"

        unless (uvmExists || uvmToolsExists) $ do
          makeChar (textToInt x) (unNvidiaDeviceId deviceId) "/dev/nvidia-uvm"
          makeChar (textToInt x) ((unNvidiaDeviceId deviceId) + 1) "/dev/nvidia-uvm-tools"
    Nvidia x -> void $ flip mapM devIds $ \deviceId -> do
        let devId = unNvidiaDeviceId deviceId
        putStrLn $ "/dev/nvidia" <> show devId
        exists <- fileExist $ "/dev/nvidia" <> show devId
        unless exists $ makeChar (textToInt x) devId ("/dev/nvidia" <> show devId)
    NvidiaModeset x -> do
        putStrLn $ "/dev/nvidia-modeset"
        exists <- fileExist $ "/dev/nvidia-modeset"
        unless exists $ makeChar (textToInt x) 254 "/dev/nvidia-modeset"

    NvidiaCaps x -> do
        putStrLn $ "/dev/nvidia-caps"
        exists <- fileExist "/dev/nvidia-caps"
        unless exists $ do
          -- More stuff lives here on workstation gpus
          createDirectory "/dev/nvidia-caps"
          setFileMode "/dev/nvidia-caps" $ ownerModes .|. groupReadMode .|. groupExecuteMode .|. otherReadMode .|. otherExecuteMode

          let cap1 = mkDev (textToInt x) 1
              cap2 = mkDev (textToInt x) 2

          createDevice "/dev/nvidia-caps/nvidia-cap1" characterSpecialMode cap1
          createDevice "/dev/nvidia-caps/nvidia-cap2" characterSpecialMode cap2

          -- These don't have 666 for some reason
          setFileMode "/dev/nvidia-caps/nvidia-cap1" ownerReadMode
          setFileMode "/dev/nvidia-caps/nvidia-cap2" $  ownerReadMode .|. groupReadMode .|. otherReadMode

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


makeChar :: Major -> Minor -> FilePath -> IO ()
makeChar maj min' fp = do
    let deviceId = mkDev maj min'
    putStrLn $ "Making char device " <> fp
    putStrLn $ "Device ID: " <> show deviceId
    createDevice fp characterSpecialMode deviceId
    setCharMode fp

mkDev :: Major -> Minor -> DeviceID
mkDev maj min' = fromIntegral $ ((maj .<<. minorbits) .|. min')
  where
      minorbits = 8


-- This might be dangerous, look into
-- Also do we care about the old special "char" mode,
-- Seems like we shouldn't and the kernel (or this library) prevents from overwriting
setCharMode :: FilePath -> IO ()
setCharMode fp = setFileMode fp mode
  where
    mode = ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. groupWriteMode .|. otherReadMode .|. otherWriteMode

textToInt :: Text -> Integer
textToInt a = case TR.decimal a of
                Left _ -> error "Couldn't parse"
                Right (i, _) -> i
