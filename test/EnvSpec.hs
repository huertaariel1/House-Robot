module EnvSpec where

import SpecHelper

spec :: Spec
spec =
  describe "meh" $ do
    {-context "with [Dirty 4 5, Dirty 2 3, Obstacle 4 5]" $
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
        initEnv 4 5 3 3 3 3 `shouldBe` [Dirty (3, 5)]-}

    {-context "get Children" $
      it "should be [Child (2,3) False,Child (1,5) False,Child (2,4) False,Child (1,3) False]" $
        getChildren
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) False, Child (1, 5) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) False, Child (1, 5) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          []
          `shouldBe` [Child (2, 3) False, Child (1, 5) False, Child (2, 4) False, Child (1, 3) False]

    context "get Children" $
      it "should be [Child (2,4) False,Child (1,3) False]" $
        getChildren
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          []
          `shouldBe` [Child (2, 4) False, Child (1, 3) False]

    context "get Children" $
      it "should be [Child (1,3) False]" $
        getChildren
          [Playpen (2, 4), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Playpen (2, 4), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          []
          `shouldBe` [Child (1, 3) False]-}

    context "valid Moves" $
      it "should be True" $
        validMove
          4
          5
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Obstacle (3, 4)]
          (0, -1)
          `shouldBe` True

    context "valid Moves" $
      it "should be False" $
        validMove
          4
          5
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Obstacle (3, 4)]
          (1, 0)
          `shouldBe` False

    context "valid Moves" $
      it "should be False" $
        validMove
          4
          5
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (3, 1) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Obstacle (3, 4)]
          (0, -1)
          `shouldBe` False

    context "pushObstacles" $
      it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 3), Obstacle (3, 2), Obstacle (3, 1)]" $
        pushObstacles
          [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          [Obstacle (3, 4)]
          (0, -1)
          `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 3), Obstacle (3, 2), Obstacle (3, 1)]

    context "gimmeSquare3X3" $
      it "should be [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]" $
        gimmeSquare3X3 4 5 (1, 5) [(0, 4), (2, 4), (1, 3), (1, 5), (2, 3), (0, 5), (0, 3), (2, 5), (1, 4)]
          `shouldBe` [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]

    context "gimmeSquare3X3" $
      it "should be [(2, 3), (4, 3), (3, 2), (2, 2), (2, 4), (4, 4), (4, 2), (3, 3)]" $
        gimmeSquare3X3 4 5 (3, 4) [(2, 3), (4, 3), (3, 2), (3, 4), (2, 2), (2, 4), (4, 4), (4, 2), (3, 3)]
          `shouldBe` [(2, 3), (4, 3), (3, 2), (2, 2), (2, 4), (4, 4), (4, 2), (3, 3)]

    context "gimmeSquare3X3Empty" $
      it "should be [(2, 5)]" $
        gimmeSquare3X3Empty [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]
          `shouldBe` [(2, 5)]

    context "gimmeSquare3X3Empty" $
      it "should be [(2, 4), (2, 3), (2, 5)]" $
        gimmeSquare3X3Empty [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]
          `shouldBe` [(2, 4), (2, 3), (2, 5)]

    context "gimmeSquare3X3C" $
      it "should be [(2, 4), (1, 3), (2, 3)]" $
        gimmeSquare3X3C [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) False, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]
          `shouldBe` [(2, 4), (1, 3), (2, 3)]

    context "gimmeSquare3X3C" $
      it "should be [(2, 4), (1, 3), (2, 3)]" $
        gimmeSquare3X3C [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [(2, 4), (1, 3), (2, 3), (2, 5), (1, 4)]
          `shouldBe` [(2, 4), (1, 3)]

    context "childrenAction" $
      it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5),  Child (2, 3) True, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]" $
        childrenAction 4 5 [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]

    {-context "leaveDirt" $
          it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (2, 3) True, Child (1, 5) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 3), Obstacle (3, 2), Obstacle (3, 1)]" $
            leaveDirt
              4
              5
              [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
              (Child (3, 3) False)
              (3, 4)
              `shouldBe` [Playpen (4, 5), Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (3, 4) False, Dirty (2, 3), Dirty (4, 3), Dirty (3, 4), Dirty (4, 4), Dirty (4, 2)]
    -}
    context "gimmeSquare3X3" $
      it "should be  [(2,3),(4,3),(3,2),(2,2),(4,4),(2,4),(4,2),(3,3)]" $
        gimmeSquare3X3 4 5 (3, 4) [(2, 3), (4, 3), (3, 2), (3, 4), (2, 2), (4, 4), (2, 4), (4, 2), (3, 3)]
          `shouldBe` [(2, 3), (4, 3), (3, 2), (2, 2), (4, 4), (2, 4), (4, 2), (3, 3)]

    context "gimmeSquare3X3Empty" $
      it "should be  [(2,3),(4,3),(4,4),(4,2)]" $
        gimmeSquare3X3Empty [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [(2, 3), (4, 3), (3, 2), (2, 2), (4, 4), (2, 4), (4, 2), (3, 3)]
          `shouldBe` [(2, 3), (4, 3), (4, 4), (4, 2)]

    context "childrenAction" $
      it "should be [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]" $
        childrenAction 4 5 [Playpen (4, 5), Child (3, 3) False, Child (2, 2) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          `shouldBe` [Playpen (4, 5), Child (3, 3) False, Child (2, 2) True, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]

    context "childrenAction" $
      it "should be [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]" $
        childrenAction 4 5 [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          `shouldBe` [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]

{-  context "putDirty" $
    it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Dirty (2, 4), Dirty (2, 3), Dirty (2, 5)]" $
      putDirty [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] 3 [(2, 4), (2, 3), (2, 5)]
        `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Dirty (2, 4), Dirty (2, 3), Dirty (2, 5)] -}
main :: IO ()
main = hspec spec
