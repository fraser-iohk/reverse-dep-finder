module ReverseDepFinder
  ( ModuleAnalysis(..)
  , analyseDirectory
  ) where

import Cabal.Plan
import Control.Foldl (Fold)
import Control.Foldl qualified as Foldl
import Control.Lens qualified as Lens
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Csv (ToNamedRecord(..), namedRecord, namedField, DefaultOrdered(..), header)
import Data.Csv.Incremental qualified as CSV
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Iface.Ext.Binary qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Types.Avail qualified as GHC
import GHC.Types.Name qualified as GHC
import GHC.Types.Name.Cache qualified as GHC
import GHC.Unit.Types qualified as GHC
import GHC.Utils.Outputable (Outputable)
import GHC.Utils.Outputable qualified as GHC
import Language.Haskell.Syntax.Module.Name qualified as Syntax
import Streaming (Stream, Of(..), liftIO)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath.Glob

data ModuleAnalysis = ModuleAnalysis
  { exports :: Set GHC.Name
  , imports :: Set Syntax.ModuleName
  , usages :: Set GHC.Name
  }

instance Outputable ModuleAnalysis where
  ppr ma = GHC.ppr (exports ma) GHC.$+$ GHC.ppr (imports ma) GHC.$+$ GHC.ppr (usages ma)

analyseDirectory :: FilePath -> Maybe FilePath -> IO ()
analyseDirectory hieDirPath cabalPlanPath = do
  pkgMap <- maybe mempty findPlanUnits cabalPlanPath
  nc <- GHC.initNameCache 'r' mempty
  let fps = findAllHiePaths hieDirPath
      hieFiles = S.map GHC.hie_file_result $ readHieFiles nc fps
  m <- toMap_ $ S.map analyseHieFile hieFiles
  let (es, is, us) = Foldl.fold (analysisToCsv pkgMap) (Map.toAscList m)
  ByteString.writeFile "exports.csv" es
  ByteString.writeFile "imports.csv" is
  ByteString.writeFile "usages.csv" us

analysisToCsv :: Map UnitId PkgId -> Fold (GHC.Module, ModuleAnalysis) (ByteString, ByteString, ByteString)
analysisToCsv pkgMap =
  (,,) <$> Foldl.premap (fmap exports) (exportsCSV pkgMap)
       <*> Foldl.premap (fmap imports) (importsCSV pkgMap)
       <*> Foldl.premap (fmap usages) (usagesCSV pkgMap)

exportsCSV :: Map UnitId PkgId -> Fold (GHC.Module, Set GHC.Name) ByteString
exportsCSV pkgMap = Foldl.foldMap (\(m, ns) -> Foldl.fold (Foldl.foldMap (encodeRow m) id) ns) CSV.encodeDefaultOrderedByName
  where
    pkgId m = Map.lookup (UnitId $ Text.pack $ GHC.unitString $ GHC.moduleUnit m) pkgMap
    encodeRow m n = CSV.encodeNamedRecord (ExportRecord (pkgId m) m n)

importsCSV :: Map UnitId PkgId -> Fold (GHC.Module, Set Syntax.ModuleName) ByteString
importsCSV pkgMap = Foldl.foldMap (\(m, ns) -> Foldl.fold (Foldl.foldMap (encodeRow m) id) ns) CSV.encodeDefaultOrderedByName
  where
    pkgId m = Map.lookup (UnitId $ Text.pack $ GHC.unitString $ GHC.moduleUnit m) pkgMap
    encodeRow m n = CSV.encodeNamedRecord (ImportRecord (pkgId m) m n)

usagesCSV :: Map UnitId PkgId -> Fold (GHC.Module, Set GHC.Name) ByteString
usagesCSV pkgMap =
  Foldl.foldMap
    (\(m, ns) ->
      Foldl.fold
        (Foldl.foldMap (encodeRow m) id)
        ns
    )
    CSV.encodeDefaultOrderedByName
  where
    findPkgId (u :: GHC.Unit) =
      Map.lookup (UnitId $ Text.pack $ GHC.unitString u) pkgMap
    pkgId m = findPkgId (GHC.moduleUnit m)
    encodeRow m n =
      CSV.encodeNamedRecord $
        UsageRecord (pkgId m) m (fmap GHC.moduleUnit (GHC.nameModule_maybe n) >>= findPkgId) n

toMap_ :: (Ord k, Monad m) => Stream (Of (k, v)) m a -> m (Map k v)
toMap_ = S.fold_ (\m (k, v) -> Map.insert k v m) mempty id

findAllHiePaths :: FilePath -> Stream (Of FilePath) IO ()
findAllHiePaths fp = liftIO (globDir1 "**/*.hie" fp) >>= S.each

readHieFiles :: GHC.NameCache -> Stream (Of FilePath) IO () -> Stream (Of GHC.HieFileResult) IO ()
readHieFiles nc = S.mapM (GHC.readHieFile nc)

analyseHieFile :: GHC.HieFile -> (GHC.Module, ModuleAnalysis)
analyseHieFile hie = do
  let asts = streamASTs (GHC.hie_asts hie)
      streamSet :: Ord a => Stream (Of a) S.Identity () -> Set a
      streamSet = S.runIdentity . S.fold_ (flip Set.insert) mempty id
      analysis = ModuleAnalysis
        { exports = astExports hie
        , imports = streamSet $ S.for asts astImports
        , usages = streamSet $ S.for asts astUsages
        }

  (GHC.hie_module hie, analysis)

astExports :: GHC.HieFile -> Set GHC.Name
astExports f = Set.fromList (GHC.hie_exports f >>= GHC.availNames)

sourceNodeInfos :: Monad m => GHC.HieAST a -> Stream (Of (GHC.NodeInfo a)) m ()
sourceNodeInfos = S.each . Map.lookup GHC.SourceInfo . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

astImports :: (Monad m, Outputable a) => GHC.HieAST a -> Stream (Of Syntax.ModuleName) m ()
astImports ast = S.for (sourceNodeInfos ast) \ni -> do
  flip Map.traverseWithKey (Map.filter isImport (GHC.nodeIdentifiers ni)) \i _d -> do
    either S.yield mempty i
  where
    isImport = any isIEThing . GHC.identInfo
    isIEThing = \case
      GHC.IEThing _iet -> True
      _ -> False

astUsages :: Monad m => GHC.HieAST a -> Stream (Of GHC.Name) m ()
astUsages ast = S.for (sourceNodeInfos ast) \ni -> do
  flip Map.traverseWithKey (Map.filter isUsage (GHC.nodeIdentifiers ni)) \i _d -> do
    case i of
      Left _ -> mempty
      Right n ->
        when (GHC.isExternalName n) do
          S.yield n
  where
    isUsage = Set.member GHC.Use . GHC.identInfo

streamASTs :: Monad m => GHC.HieASTs a -> Stream (Of (GHC.HieAST a)) m ()
streamASTs = S.each . (>>= astUniverse) . Map.elems . GHC.getAsts

astUniverse :: GHC.HieAST a -> [GHC.HieAST a]
astUniverse = Lens.universeOf (Lens.folding GHC.nodeChildren)

_showPpr :: Outputable a => a -> String
_showPpr = GHC.renderWithContext GHC.defaultSDocContext . GHC.ppr

data ExportRecord = ExportRecord (Maybe PkgId) GHC.Module GHC.Name

instance DefaultOrdered ExportRecord where
  headerOrder _ = header
    [ "Package name"
    , "Package version"
    , "Module unit"
    , "Module name"
    , "Symbol"
    , "Source module unit"
    , "Source module name"
    ]

instance ToNamedRecord ExportRecord where
  toNamedRecord (ExportRecord pkgId m n) = do
    let
      symbolModuleUnit = (GHC.unitString . GHC.moduleUnit) <$> GHC.nameModule_maybe n
      symbolModuleName = (Syntax.moduleNameString . GHC.moduleName) <$> GHC.nameModule_maybe n
    namedRecord
      [ namedField "Package name" $ pkgIdNameText <$> pkgId
      , namedField "Package version" $ pkgIdVerText <$> pkgId
      , namedField "Module unit" $ GHC.unitString $ GHC.moduleUnit m
      , namedField "Module name" $ Syntax.moduleNameString (GHC.moduleName m)
      , namedField "Symbol" $ GHC.getOccString n
      , namedField "Source module unit" $ symbolModuleUnit
      , namedField "Source module name" $ symbolModuleName
      ]

data ImportRecord = ImportRecord (Maybe PkgId) GHC.Module Syntax.ModuleName

instance DefaultOrdered ImportRecord where
  headerOrder _ = header
    [ "Package name"
    , "Package version"
    , "Module unit"
    , "Module name"
    , "Import"
    ]

instance ToNamedRecord ImportRecord where
  toNamedRecord (ImportRecord pkgId m mn) = do
    let
    namedRecord
      [ namedField "Package name" $ pkgIdNameText <$> pkgId
      , namedField "Package version" $ pkgIdVerText <$> pkgId
      , namedField "Module unit" $ GHC.unitString $ GHC.moduleUnit m
      , namedField "Module name" $ Syntax.moduleNameString $ GHC.moduleName m
      , namedField "Import" $ Syntax.moduleNameString mn
      ]

data UsageRecord =
  UsageRecord (Maybe PkgId) GHC.Module (Maybe PkgId) GHC.Name

instance DefaultOrdered UsageRecord where
  headerOrder _ = header
    [ "Package name"
    , "Package version"
    , "Module unit"
    , "Module name"
    , "Symbol"
    , "Source package name"
    , "Source package version"
    , "Source module unit"
    , "Source module name"
    ]

instance ToNamedRecord UsageRecord where
  toNamedRecord (UsageRecord pkgId m sourcePkgId n) = do
    let
      symbolModuleUnit = (GHC.unitString . GHC.moduleUnit) <$> GHC.nameModule_maybe n
      symbolModuleName = (Syntax.moduleNameString . GHC.moduleName) <$> GHC.nameModule_maybe n
    namedRecord
      [ namedField "Package name" $ pkgIdNameText <$> pkgId
      , namedField "Package version" $ pkgIdVerText <$> pkgId
      , namedField "Module unit" $ GHC.unitString $ GHC.moduleUnit m
      , namedField "Module name" $ Syntax.moduleNameString $ GHC.moduleName m
      , namedField "Symbol" $ GHC.getOccString n
      , namedField "Source package name" $ pkgIdNameText <$> sourcePkgId
      , namedField "Source package version" $ pkgIdVerText <$> sourcePkgId
      , namedField "Source module unit" symbolModuleUnit
      , namedField "Source module name" symbolModuleName
      ]


pkgIdNameText :: PkgId -> Text
pkgIdNameText (PkgId (PkgName n) _) = n

pkgIdVerText :: PkgId -> Text
pkgIdVerText (PkgId _ v) = dispVer v

findPlanUnits :: FilePath -> IO (Map UnitId PkgId)
findPlanUnits fp = do
  pj <- decodePlanJson fp
  pure $ uPId <$> pjUnits pj
