module Test.Transit.Render.Theme
  ( spec
  ) where

import Prelude

import Color as Color
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Render.Theme (getColorHarmony, themeContrastDark, themeContrastLight, themeGradientDark, themeGradientLight, themeHarmonyDark, themeHarmonyLight)

spec :: Spec Unit
spec = do
  describe "Transit.Render.Theme" do
    describe "getColorHarmony" do
      it "returns color harmony by index" do
        let harmony = getColorHarmony themeHarmonyLight 0
        -- Verify it returns a ColorHarmony
        harmony.nodeBg `shouldEqual` harmony.nodeBg

      it "cycles through harmonies when index exceeds array length" do
        let
          harmony1 = getColorHarmony themeHarmonyLight 0
          harmony2 = getColorHarmony themeHarmonyLight 11
        -- Should cycle back to first harmony
        harmony1.nodeBg `shouldEqual` harmony2.nodeBg

      it "handles negative indices by cycling" do
        let
          lastIndex = 10
          harmonyLast = getColorHarmony themeHarmonyLight lastIndex
          harmonyNeg1 = getColorHarmony themeHarmonyLight (-1)
        -- Should cycle to last harmony
        harmonyLast.nodeBg `shouldEqual` harmonyNeg1.nodeBg

    describe "themeHarmonyLight" do
      it "has white background" do
        themeHarmonyLight.bgColor `shouldEqual` Color.rgb 255 255 255

      it "has black title color" do
        themeHarmonyLight.titleColor `shouldEqual` Color.rgb 0 0 0

      it "has multiple color harmonies" do
        -- Should have harmonies for all base colors
        (getColorHarmony themeHarmonyLight 0).nodeBg `shouldEqual` (getColorHarmony themeHarmonyLight 0).nodeBg

    describe "themeHarmonyDark" do
      it "has dark background" do
        themeHarmonyDark.bgColor `shouldEqual` Color.rgb 20 20 20

      it "has light title color" do
        themeHarmonyDark.titleColor `shouldEqual` Color.rgb 200 200 200

    describe "themeContrastLight" do
      it "has white background" do
        themeContrastLight.bgColor `shouldEqual` Color.white

      it "has black title color" do
        themeContrastLight.titleColor `shouldEqual` Color.black

      it "has single color harmony" do
        let
          harmony1 = getColorHarmony themeContrastLight 0
          harmony2 = getColorHarmony themeContrastLight 1
        -- Should be the same harmony (only one)
        harmony1.nodeBg `shouldEqual` harmony2.nodeBg

    describe "themeContrastDark" do
      it "has black background" do
        themeContrastDark.bgColor `shouldEqual` Color.black

      it "has white title color" do
        themeContrastDark.titleColor `shouldEqual` Color.white

    describe "themeGradientLight" do
      it "has white background" do
        themeGradientLight.bgColor `shouldEqual` Color.rgb 255 255 255

      it "has single color harmony" do
        let
          harmony1 = getColorHarmony themeGradientLight 0
          harmony2 = getColorHarmony themeGradientLight 5
        -- Should be the same harmony
        harmony1.nodeBg `shouldEqual` harmony2.nodeBg

    describe "themeGradientDark" do
      it "has black background" do
        themeGradientDark.bgColor `shouldEqual` Color.rgb 0 0 0

      it "has white title color" do
        themeGradientDark.titleColor `shouldEqual` Color.rgb 255 255 255

