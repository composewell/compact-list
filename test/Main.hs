import Data.CompactList
import Control.Monad (replicateM, void)

main :: IO ()
main = do
    ref <- newCompactList []
    void $ replicateM 1000000 $ do
        consCompactList ref ()
        return ()
    readCompactList ref >>= print . length
