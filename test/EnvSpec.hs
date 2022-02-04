module EnvSpec where

import Data.Matrix
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
        initEnv 6 6 3 5 3 3 `shouldBe` [Dirty (3, 5)]-}

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

    {- context "valid Moves" $
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
    -}
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
    {-context "gimmeSquare3X3" $
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
          `shouldBe` [Playpen (4, 5), Child (3, 3) False, Child (2, 2) False, Child (2, 4) False, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] -}

    {-context "putDirty" $
      it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Dirty (2, 4), Dirty (2, 3), Dirty (2, 5)]" $
        putDirty [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] 3 [(2, 4), (2, 3), (2, 5)]
          `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Dirty (2, 4), Dirty (2, 3), Dirty (2, 5)]-}

    context "getDirty" $
      it "should be [Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]" $
        getDirty [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] []
          `shouldBe` [Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]

    context "getRobots" $
      it "should be [ Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False]" $
        getRobots [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] []
          `shouldBe` [Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False]

    context "getPlaypen" $
      it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5)]" $
        getPlaypen [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (1, 3) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] []
          `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5)]

    context "getPlaypen" $
      it "should be [Playpen (4, 2), Playpen (4, 3), Playpen (4, 5)]" $
        getPlaypen [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (4, 4) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [Playpen (4, 2), Playpen (4, 3), Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (4, 4) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] []
          `shouldBe` [Playpen (4, 2), Playpen (4, 3), Playpen (4, 5)]

    context "getPlaypen" $
      it "should be []" $
        getPlaypen [Playpen (4, 4), Playpen (4, 5), Child (4, 5) True, Child (4, 4) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] [Playpen (4, 4), Playpen (4, 5), Child (1, 5) True, Child (4, 4) False, Robot (1, 2) False, Robot (1, 1) False, Robot (2, 1) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] []
          `shouldBe` []

    context "bfs" $
      it "should be\n┌                ┐\n│  1  0  1  2  3 │\n│  2  1  2  3  4 │\n│  3 -2 -2 -2  5 │\n│  4  5  6 -2 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) True, Child (4, 4) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] (1, 2)
          `shouldBe` let m = fromList 4 5 [1, 0, 1, 2, 3, 2, 1, 2, 3, 4, 3, -2, -2, -2, 5, 4, 5, 6, -2, -2] in m

    context "bfs" $
      it "should be\n┌                ┐\n│  1  0  1  2  3 │\n│  2  1  2  3  4 │\n│ -2  2 -2 -2  5 │\n│  4  3  4  5 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] (1, 2)
          `shouldBe` let m = fromList 4 5 [1, 0, 1, 2, 3, 2, 1, 2, 3, 4, -2, 2, -2, -2, 5, 4, 3, 4, 5, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Child (3,2) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Child (3, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (2, 2) False]

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False,Robot (3,2) True,Child (3,2) True]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Child (3, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (2, 2) False]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (3, 2) True, Child (3, 2) True]

    context "bfs" $
      it "should be\n┌                ┐\n│  3  2  3  4  5 │\n│  2  1  2  3  4 │\n│ -2  0 -2 -2  5 │\n│  2  1  2  3 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (3, 2) True, Child (3, 2) True] (3, 2)
          `shouldBe` let m = fromList 4 5 [3, 2, 3, 4, 5, 2, 1, 2, 3, 4, -2, 0, -2, -2, 5, 2, 1, 2, 3, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Robot (4,3) True,Child (4,3) True]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (3, 2) True, Child (3, 2) True]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 3) True, Child (4, 3) True]

    context "bfs" $
      it "should be\n┌                ┐\n│  5  4  5  6  7 │\n│  4  3  4  5  6 │\n│ -2  2 -2 -2  7 │\n│  2  1  0  1 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 3) True, Child (4, 3) True] (4, 3)
          `shouldBe` let m = fromList 4 5 [5, 4, 5, 6, 7, 4, 3, 4, 5, 6, -2, 2, -2, -2, 7, 2, 1, 0, 1, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Robot (4,4) True,Child (4,4) True]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 3) True, Child (4, 3) True]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) True, Child (4, 4) True]

    context "bfs" $
      it "should be\n┌                ┐\n│  5  4  5  6  7 │\n│  4  3  4  5  6 │\n│ -2  2 -2 -2  7 │\n│  2  1  0 -2 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) True, Child (4, 4) True] (4, 3)
          `shouldBe` let m = fromList 4 5 [5, 4, 5, 6, 7, 4, 3, 4, 5, 6, -2, 2, -2, -2, 7, 2, 1, 0, -2, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Robot (4,4) True,Child (4,4) True]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) True, Child (4, 4) True]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) False, Child (4, 4) False]

    context "bfs" $
      it "should be\n┌                ┐\n│  6  5  6  7  8 │\n│  5  4  5  6  7 │\n│ -2  3 -2 -2  8 │\n│  3  2  1  0 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) False, Child (4, 4) False] (4, 4)
          `shouldBe` let m = fromList 4 5 [6, 5, 6, 7, 8, 5, 4, 5, 6, 7, -2, 3, -2, -2, 8, 3, 2, 1, 0, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Child (4,4) True, Robot (4, 3) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Robot (4, 4) False, Child (4, 4) False]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 3) False]

    context "bfs" $
      it "should be\n┌                ┐\n│  5  4  5  6  7 │\n│  4  3  4  5  6 │\n│ -2  2 -2 -2  7 │\n│  2  1  0 -2 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 3) False] (4, 3)
          `shouldBe` let m = fromList 4 5 [5, 4, 5, 6, 7, 4, 3, 4, 5, 6, -2, 2, -2, -2, 7, 2, 1, 0, -2, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Child (4,4) True, Robot (4, 2) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 3) False]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 2) False]

    context "bfs" $
      it "should be\n┌                ┐\n│  5  4  5  6  7 │\n│  4  3  4  5  6 │\n│ -2  2 -2 -2  7 │\n│  2  1  0 -2 -2 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 2) False] (4, 2)
          `shouldBe` let m = fromList 4 5 [4, 3, 4, 5, 6, 3, 2, 3, 4, 5, -2, 1, -2, -2, 6, 1, 0, 1, -2, -2] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Robot (2,2) False, Child (4,4) True, Robot (4, 1) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 2) False]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 1) False]

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Robot (2,2) False, Child (4,4) True, Robot (4, 1) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Child (4, 4) False, Robot (4, 1) False]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 5) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Child (4, 4) False, Robot (4, 1) False]

    context "bfs" $
      it "should be\n┌                ┐\n│  1  0  1  2  3 │\n│  2  1  2  3  4 │\n│ -2  2 -2 -2 -2 │\n│  4  3  4 -2 -1 │\n└                ┘" $
        bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 4) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 5)] (1, 2)
          `shouldBe` let m = fromList 4 5 [1, 0, 1, 2, 3, 2, 1, 2, 3, 4, -2, 2, -2, -2, -2, 4, 3, 4, -2, -1] in m

    context "go Robot" $
      it "should be [Playpen (4,4),Playpen (4,5),Child (4,5) False,Child (3,2) False,Obstacle (3,3),Obstacle (3,4),Obstacle (3,1),Dirty (1,4),Dirty (3,5),Dirty (4,1),Obstacle (3, 5), Robot (1, 3) False]" $
        robotAction 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 4) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 5)]
          `shouldBe` [Playpen (4, 4), Playpen (4, 5), Child (4, 4) False, Child (3, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 5), Robot (1, 3) False]

    context "board Print" $
      it "should be\n┌                ┐\n│  -  R  -  D  - │\n│  -  -  -  -  - │\n│  O  C  O  O DO │\n│  D  -  - PC  C │\n└                ┘" $
        let sMatrix = matrix 4 5 $ \(i, j) -> "---"
         in boardPrint [Playpen (4, 4), Playpen (4, 5), Child (4, 4) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 1), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Obstacle (3, 5)] sMatrix
              `shouldBe` let m = fromList 4 5 ["---", "R", "---", "D", "---", "---", "---", "---", "---", "---", "O", "C", "O", "O", "DO", "D", "---", "---", "PC", "P"] in m

main :: IO ()
main = hspec spec
