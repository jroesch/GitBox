module GitCore where
  
import Foreign.Ptr
import Foreign.C.String
import Bindings.Libgit2
import Bindings.Libgit2.Repository
import Bindings.Libgit2.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent

data Repository = Repository { ptr :: (ForeignPtr (Ptr C'git_repository)) }  deriving (Show, Eq)

type Error = Integer
type Result a = Either Error a

mkRepositoryPtr :: Ptr (Ptr C'git_repository) -> IO (ForeignPtr (Ptr C'git_repository))
mkRepositoryPtr ptr = newForeignPtr ptr $ do
  innerPtr <- peek ptr
  c'git_repository_free innerPtr

openRepository :: String -> IO (Result Repository)
openRepository path = do
  cpath <- newCString path
  repo <- malloc
  err <- c'git_repository_open repo cpath
  case toInteger err of
    0 -> do 
      fptr <- mkRepositoryPtr repo 
      return $ Right $ Repository fptr
    e -> return $ Left e


test = withLibGitDo $ do
  repoOrErr <- openRepository "/Users/jroesch/Projects/GitBox"
  case repoOrErr of
    Left err -> error $ "Failed with code: " ++ (show err) ++ "\n"
    Right repo ->
      return repo

repositoryPath repo =
  withForeignPtr (ptr repo) $ \r -> do
    rptr <- peek r
    cstr <- c'git_repository_path rptr
    peekCString cstr
      

      
