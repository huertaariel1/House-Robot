module EnvSpec where

import SpecHelper

spec :: Spec
spec =
  describe "meh" $ do
    context "with [Dirty 4 5, Dirty 2 3, Obstacle 4 5]" $
      it "should be [Dirty 4 5, Robot (4, 5) False, Obstacle (4, 5)]" $
        index [Dirty (4, 5), Dirty (2, 3), Robot (4, 5) False, Obstacle (4, 5)] (4, 5) `shouldBe` [Dirty (4, 5), Robot (4, 5) False, Obstacle (4, 5)]

    context "with [Dirty 4 5, Dirty 2 3, Obstacle 4 5]" $
      it "should be [Dirty 4 5, Obstacle (4, 5)]" $
        index [Dirty (4, 5), Dirty (2, 3), Obstacle (4, 5)] (4, 5) `shouldBe` [Dirty (4, 5), Obstacle (4, 5)]

    context "with [Dirty 4 5, Dirty 2 3, Obstacle 2 5]" $
      it "should be [Dirty 4 5]" $
        index [Dirty (4, 5), Dirty (2, 3), Obstacle (2, 5)] (4, 5) `shouldBe` [Dirty (4, 5)]

    context "with [Dirty 4 5, Dirty 2 3, Obstacle 2 5]" $
      it "should be [Dirty 4 5, Obstacle 2 5]" $
        removeCell (Dirty (2, 3)) [Dirty (4, 5), Dirty (2, 3), Obstacle (2, 5)] `shouldBe` [Dirty (4, 5), Obstacle (2, 5)]

    context "with [Dirty 4 5, Dirty 2 3, Robot (4, 5) False, Obstacle (4 5)] Robot (4, 6) True" $
      it "should be [Dirty 4 5, Dirty 2 3, Obstacle (4 5), Robot (4, 6) True]" $
        updateCell [Dirty (4, 5), Dirty (2, 3), Robot (4, 5) False, Obstacle (4, 5)] (Robot (4, 5) False) (Robot (4, 6) True) `shouldBe` [Dirty (4, 5), Dirty (2, 3), Obstacle (4, 5), Robot (4, 6) True]

    context "with 2 3 3 0" $
      it "should be False" $
        boundsEnv 2 3 3 0 `shouldBe` False

    context "with 2 3 2 -1" $
      it "should be False" $
        boundsEnv 2 3 3 (-1) `shouldBe` False

    context "with 2 3 1 1" $
      it "should be True" $
        boundsEnv 2 3 1 1 `shouldBe` True

    context "with taken [Dirty 4 5, Dirty 2 3, Robot (4, 5) False, Obstacle (4 5)] (4, 6)" $
      it "should be False" $
        taken [Dirty (4, 5), Dirty (2, 3), Robot (4, 5) False, Obstacle (4, 5)] (4, 6) `shouldBe` False

    context "with taken [Dirty 4 5, Dirty 2 3, Robot (4, 5) False, Obstacle (4 5)] (4, 5)" $
      it "should be True" $
        taken [Dirty (4, 5), Dirty (2, 3), Robot (4, 5) False, Obstacle (4, 5)] (4, 5) `shouldBe` True

    context "with placePlaypen" $
      it "should be whatever" $
        placePlaypen 4 5 2 `shouldBe` [Dirty (3, 5)]

    context "with placeChild" $
      it "should be whatever" $
        placeChilds 4 5 2 [Dirty (3, 5)] `shouldBe` [Dirty (3, 5)]

    context "with initEnv" $
      it "should be whatever" $
        initEnv 4 5 3 2 3 3 `shouldBe` [Dirty (3, 5)]

main :: IO ()
main = hspec spec