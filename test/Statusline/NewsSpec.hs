module Statusline.NewsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Statusline.News
import Test.Hspec

rss :: Text
rss =
  T.concat
    [ "<rss><channel><title>NHKニュース</title>"
    , "<image><title>logo</title></image>"
    , "<item><title>一つ目の見出し</title><link>a</link></item>"
    , "<item><title> 二つ目 </title></item>"
    , "</channel></rss>"
    ]

spec :: Spec
spec = describe "newsTitles" $ do
  it "extracts item titles only, skipping channel and image titles" $
    newsTitles rss `shouldBe` ["一つ目の見出し", "二つ目"]
  it "unwraps CDATA" $
    newsTitles "<item><title><![CDATA[見出し]]></title></item>"
      `shouldBe` ["見出し"]
  it "resolves XML entities" $
    newsTitles "<item><title>A &amp; B &lt;C&gt;</title></item>"
      `shouldBe` ["A & B <C>"]
  it "skips items without a title" $
    newsTitles "<item><link>x</link></item><item><title>t</title></item>"
      `shouldBe` ["t"]
  it "empty input -> no titles" $ newsTitles "" `shouldBe` []
  it "malformed input -> no titles" $ newsTitles "not xml at all" `shouldBe` []
  it "unterminated title -> takes the remainder" $
    newsTitles "<item><title>never closed" `shouldBe` ["never closed"]
