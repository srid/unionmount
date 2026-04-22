module System.UnionMount.UnitSpec where

import Data.List.NonEmpty qualified as NE
import System.UnionMount qualified as UM
import Test.Hspec

spec :: Spec
spec = do
  describe "chainM" $ do
    it "identity on empty input" $ do
      f <- UM.chainM (\x -> pure (+ x)) ([] :: [Int])
      f (10 :: Int) `shouldBe` 10
    it "applies functions in left-to-right order" $ do
      -- Each element produces a function that appends itself; left-to-right
      -- order means the first element is applied first.
      f <- UM.chainM (\c -> pure (<> [c])) "abc"
      f "" `shouldBe` "abc"
    it "monadic effects run once per element" $ do
      ref <- newIORef (0 :: Int)
      _ <- UM.chainM (\_ -> modifyIORef' ref (+ 1) >> pure (id :: Int -> Int)) [(), (), ()]
      readIORef ref `shouldReturn` 3

  describe "getTag" $ do
    let pats = [(1 :: Int, "*.md"), (2, "*.txt")]
    it "tags a matching relative path" $ do
      UM.getTag pats "foo.md" `shouldBe` Just 1
      UM.getTag pats "foo.txt" `shouldBe` Just 2
    it "returns Nothing when no pattern matches" $ do
      UM.getTag pats "foo.hs" `shouldBe` Nothing
    it "picks the first matching pattern" $ do
      -- Overlapping patterns: first one wins.
      let overlap = [(1 :: Int, "*.md"), (2, "foo.md")]
      UM.getTag overlap "foo.md" `shouldBe` Just 1
    it "matches absolute paths via an implicit **/ prefix" $ do
      UM.getTag pats "/tmp/some/where/foo.md" `shouldBe` Just 1

  describe "OverlayFs" $ do
    let ofs0 = UM.emptyOverlayFs :: UM.OverlayFs String
    it "lookup on empty returns Nothing" $ do
      UM.overlayFsLookup "file" ofs0 `shouldBe` Nothing
    it "add then lookup returns the single source" $ do
      let ofs = UM.overlayFsAdd "file" ("A", "file") ofs0
      fmap NE.toList (UM.overlayFsLookup "file" ofs)
        `shouldBe` Just [(("A", "file"), "file")]
    it "lookup of unrelated file still returns Nothing" $ do
      let ofs = UM.overlayFsAdd "file" ("A", "file") ofs0
      UM.overlayFsLookup "other" ofs `shouldBe` Nothing
    it "multiple sources are all returned" $ do
      let ofs =
            UM.overlayFsAdd "file" ("A", "file") $
              UM.overlayFsAdd "file" ("B", "file") ofs0
      fmap (sort . NE.toList) (UM.overlayFsLookup "file" ofs)
        `shouldBe` Just [(("A", "file"), "file"), (("B", "file"), "file")]
    it "remove deletes a single source, leaving others" $ do
      let ofs =
            UM.overlayFsRemove "file" ("B", "file") $
              UM.overlayFsAdd "file" ("A", "file") $
                UM.overlayFsAdd "file" ("B", "file") ofs0
      fmap NE.toList (UM.overlayFsLookup "file" ofs)
        `shouldBe` Just [(("A", "file"), "file")]
    it "remove of last source makes lookup return Nothing" $ do
      let ofs =
            UM.overlayFsRemove "file" ("A", "file") $
              UM.overlayFsAdd "file" ("A", "file") ofs0
      UM.overlayFsLookup "file" ofs `shouldBe` Nothing
    it "remove of a non-existent source is a no-op" $ do
      let ofs =
            UM.overlayFsRemove "file" ("B", "file") $
              UM.overlayFsAdd "file" ("A", "file") ofs0
      fmap NE.toList (UM.overlayFsLookup "file" ofs)
        `shouldBe` Just [(("A", "file"), "file")]
