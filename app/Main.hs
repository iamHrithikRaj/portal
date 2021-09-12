{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Turtle -- the library for interfacing with the OS
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path

import qualified Data.Aeson as JSON -- reading and writing JSON files (setting.json)
import Data.Aeson ((.=), (.:))

import Options.Applicative
import Control.Monad
import Data.Traversable
import Data.Maybe
import Data.List

import qualified Data.Text as T -- Text uses a more efficient representation of text
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.ByteString.Lazy as B -- read and write JSON files onto the filesystem

import qualified System.Console.ANSI as ANSI --  ANSI library is used for coloring our outputs.

portalProgDesc :: String
portalProgDesc = "use portal to setup portal points and move to these " ++
               "when needed"

portalHeader :: String
portalHeader = "portal: move around your filesystem"

-- the combined datatype representing all tp commands
data Command = CommandList |
               CommandAdd {
                    addName :: String,
                    folderPath :: FilePath
               } |
               CommandRemove {
                    removeName :: String
               } |
               CommandGoto {
                   gotoName :: String
                }
    deriving (Show)


-- | A version of 'execParser' which shows full help on error.
--
-- The regular 'execParser' only prints usage on error, which doesn't
-- include the options, subcommands, or mention of the help switch
-- @--help@.
-- helper :: Parser (a -> a)
-- info :: Parser a -> InfoMod a -> ParserInfo a
-- fullDesc :: InfoMod a
-- progDesc :: String -> InfoMod a
-- header :: String -> InfoMod a

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = do
    -- command :: Command
    command <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc portalProgDesc <>
                        header portalHeader))
    -- run :: IO ()
    run command


parseCommand :: Parser Command
parseCommand = subparser $
    -- add command
    (command
        "add" -- command name
        (info -- attach help information to the parser
            (helper <*> parseAddCommand) -- core parser with the --help option
            (fullDesc <> progDesc "add a portal point") -- description of command (for info)
        )
    )
    <> -- combine with the next command

    -- list command
    (command "list"
        (info (helper <*> parseListCommand)
        (fullDesc <> progDesc "list all portal points"))
    ) <>
    -- remove command
    (command "remove"
        (info (helper <*> parseRemoveCommand)
        (fullDesc <>progDesc "remove a portal point"))
    ) <>
    -- goto command
    (command "goto"
        (info (helper <*> parseGotoCommand)
        (fullDesc <> progDesc "go to a created portal"))
    )

-- Command parsers
-- """""""""""""""

-- List
-- ----
-- $ tp list
parseListCommand :: Parser Command
parseListCommand = pure (CommandList)


parseAddCommand :: Parser Command
parseAddCommand = 
                   (liftA2
                        CommandAdd  -- :: String -> FilePath -> Commandd
                        portalnameParser -- :: Parser String
                        folderParser -- :: Parser FilePath
                   )

-- Warp Name parser
-- """"""""""""""""
portalnameParser :: Parser String
portalnameParser = argument  -- :: ReadM String -> Mod ArgumentFields String -> Parser String
                  str -- :: ReadM String
                  (metavar -- :: String -> Mod ArgumentFields String
                    "NAME" <>
                  help -- :: String -> Mod ArgumentFields String
                    "name of the portal for usage") -- Mod ArgumentFields String


-- take a string, parse it into a folder path.
-- if path does not exist, return an error
readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
      then return path
      else readerError ("invalid path: " ++ (show path))

-- Folder Parser
-- """"""""""""""
folderParser :: Parser FilePath
folderParser = argument
              (str -- :: ReadM String
                >>=
               readFolderPath) -- :: String -> ReadM FilePath
              (value "./"  <>
              metavar "FOLDERPATH" <>
              help ("path of the portal folder to create portal to." ++ 
                   "By default, taken as current working directory"))

parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap CommandRemove portalnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = fmap CommandGoto portalnameParser

-- an abstract entity representing a point to which we can tp to
data PortalPoint = PortalPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


instance JSON.FromJSON PortalPoint where
     parseJSON (JSON.Object json) =
        liftA2 PortalPoint (json .: "name")
                  (json .: "absFolderPath")

instance JSON.ToJSON PortalPoint where
    toJSON (PortalPoint {..}) =
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]


-- the main data that is loaded from JSON
data PortalData = PortalData {
    portalPoints :: [PortalPoint]
} deriving (Show)

instance JSON.FromJSON PortalData where
    parseJSON (JSON.Object v) =
        fmap PortalData (v .: "portalPoints")

instance JSON.ToJSON PortalData where
    toJSON(PortalData{..}) = 
        JSON.object ["portalPoints" .= portalPoints]

defaultPortalData :: PortalData
defaultPortalData = PortalData {
    portalPoints = []
}

filePathToString :: FilePath -> String
filePathToString = Path.encodeString

-- Data Loading
-- """"""""""""

dieJSONParseError :: FilePath -> String -> IO a
dieJSONParseError jsonFilePath err = do
    let errorstr = ("parse error in: " ++ (show jsonFilePath) ++
                    "\nerror:------\n" ++ err)
    Turtle.die (T.pack errorstr)

decodePortalData :: FilePath -> IO PortalData
decodePortalData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json


createPortalDataFile :: FilePath -> IO ()
createPortalDataFile jsonFilePath = savePortalData jsonFilePath defaultPortalData

loadPortalData :: FilePath -> IO PortalData
loadPortalData jsonFilePath = do
    exists <- (Turtle.testfile jsonFilePath)
    if exists then
        decodePortalData jsonFilePath
    else
       do
           createPortalDataFile jsonFilePath
           return defaultPortalData

savePortalData :: FilePath -> PortalData -> IO ()
savePortalData jsonFilePath tpData = do
    let dataBytestring = JSON.encode tpData
    Turtle.touch jsonFilePath
    B.writeFile (filePathToString jsonFilePath) dataBytestring


getPortalDataPath :: IO FilePath
getPortalDataPath = do
    homeFolder <- Turtle.home
    return $ homeFolder </> ".portaldata"


-- Stream Helpers
-- """"""""""""""

-- set terminal to output error color
setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [-- color to set
                             ANSI.SetColor
                             -- wherther foreground / background should be affected
                             ANSI.Foreground
                             -- use the "vivid" color versus the muted colord
                             ANSI.Vivid
                             -- use red
                             ANSI.Red
                            ]    


-- print a portal point to stdout
portalPointPrint :: PortalPoint -> IO ()
portalPointPrint portalPoint = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    putStr (name portalPoint)
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (absFolderPath portalPoint)
    putStr "\n"

-- error out that the given folder is not found
folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    setErrorColor  
    let errorstr = T.pack ("unable to find folder: " ++ (show path)) 
    Turtle.die errorstr

-- error out that folder is required, but path points
-- to a file
needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
    setErrorColor
    let errorstr = T.pack ("expected folder, not file: " ++ (show path)) 
    Turtle.die errorstr

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = 
    do
        folderExists <- Turtle.testdir path
        fileExists <- Turtle.testfile path
        -- error checking
        when fileExists (needFolderNotFileError path)
        unless folderExists (folderNotFoundError path)
       -- we know the folder exists

-- error out that the portal point already exists
diePortalPointExists :: PortalPoint -> IO ()
diePortalPointExists portalPoint  =  do
    setErrorColor
    putStrLn ("portal point " ++ (name portalPoint) ++ " already exists:\n")
    portalPointPrint portalPoint
    Turtle.die ""



-- Add command runner
-- """"""""""""""""""

runAdd :: FilePath -> String -> IO ()
runAdd folderPath addname = do
    dieIfFolderNotFound folderPath
    portalDataPath <- getPortalDataPath
    portalData <- loadPortalData portalDataPath
    absFolderPath <- Turtle.realpath folderPath

    let existingPortalPoint = find (\portal -> name portal == addname) (portalPoints portalData)
    case existingPortalPoint of
        Just portalPoint -> diePortalPointExists portalPoint
        Nothing -> do
                        let newPortalPoint = PortalPoint {
                            name=addname,
                            absFolderPath=filePathToString absFolderPath
                        }

                        putStrLn "creating portal point: \n"
                        portalPointPrint newPortalPoint

                        let newPortalData = PortalData {
                             portalPoints= newPortalPoint:(portalPoints portalData)   
                        }

                        savePortalData portalDataPath newPortalData


-- List Command
-- """"""""""""

runList :: IO ()
runList = do
    portalDataPath <- getPortalDataPath
    portalData <- loadPortalData portalDataPath
    let num_points = length $ portalPoints portalData
    putStr "portal points: "

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] 
    putStr $ "(total " <> (show num_points) <>  ")\n"
    forM_ (portalPoints portalData) portalPointPrint


-- Remove Command
-- """""""""""""""

diePortalPointNotFound :: String ->IO ()
diePortalPointNotFound name = do
    setErrorColor
    let errorname = T.pack (name ++ " portal point not found")
    Turtle.die errorname

runRemove :: String -> IO ()
runRemove removeName = do
    portalDataPath <- getPortalDataPath
    portalData <- loadPortalData portalDataPath

    let wantedPortalPoint = find (\portal -> name portal== removeName) (portalPoints portalData)
    case wantedPortalPoint of
        Nothing -> diePortalPointNotFound removeName
        Just _ ->  do
                    let newPortalPoints = filter (\portal -> name portal /= removeName)
                                               (portalPoints portalData)
                    let newPortalData = portalData {
                        portalPoints = newPortalPoints
                    }

                    savePortalData portalDataPath newPortalData
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Dull ANSI.White]    
                    putStr "removed portal point ["
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Vivid ANSI.Blue]    
                    putStr removeName
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Dull ANSI.White]    
                    putStr "]"


runGoto :: String -> IO ()
runGoto gotoName = do
    portalDataPath <- getPortalDataPath
    portalData <- loadPortalData portalDataPath

    let wantedPortalPoint = find (\portal -> name portal == gotoName) (portalPoints portalData)
    case wantedPortalPoint of
        Nothing -> diePortalPointNotFound gotoName
        Just portalPoint -> do
                             Turtle.echo (Turtle.unsafeTextToLine (T.pack (absFolderPath portalPoint)))
                             Turtle.exit (Turtle.ExitFailure 2) 


run :: Command -> IO ()
run command = 
    case command of
        CommandAdd{..} -> runAdd folderPath addName
        CommandList -> runList
        CommandRemove{..} -> runRemove removeName
        CommandGoto{..} -> runGoto gotoName