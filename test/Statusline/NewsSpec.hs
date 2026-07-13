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

titles :: Text -> [Text]
titles = map niTitle . newsItems

spec :: Spec
spec = do
  describe "newsItems (titles)" $ do
    it "extracts item titles only, skipping channel and image titles" $
      titles rss `shouldBe` ["一つ目の見出し", "二つ目"]
    it "unwraps CDATA" $
      titles "<item><title><![CDATA[見出し]]></title></item>"
        `shouldBe` ["見出し"]
    it "resolves XML entities" $
      titles "<item><title>A &amp; B &lt;C&gt;</title></item>"
        `shouldBe` ["A & B <C>"]
    it "skips items without a title" $
      titles "<item><link>x</link></item><item><title>t</title></item>"
        `shouldBe` ["t"]
    it "empty input -> no items" $ newsItems "" `shouldBe` []
    it "malformed input -> no items" $ newsItems "not xml at all" `shouldBe` []
    it "unterminated title -> takes the remainder" $
      titles "<item><title>never closed" `shouldBe` ["never closed"]
    it "strips control characters" $
      titles "<item><title>a\ESCb\ac</title></item>" `shouldBe` ["abc"]
    it "strips bidi overrides that could reorder the row" $
      titles "<item><title>a\x202E\&b\x2066\&c</title></item>" `shouldBe` ["abc"]
    it "keeps ZWJ so emoji sequences survive" $
      titles "<item><title>\x1F468\x200D\x1F469</title></item>"
        `shouldBe` ["\x1F468\x200D\x1F469"]
    it "strips the Google News \" - <publisher>\" suffix named in <source>" $
      titles "<item><title>見出し - 日本経済新聞</title><source url=\"https://www.nikkei.com\">日本経済新聞</source></item>"
        `shouldBe` ["見出し"]
    it "leaves the title alone when there is no <source> tag" $
      titles "<item><title>見出し - 日本経済新聞</title></item>"
        `shouldBe` ["見出し - 日本経済新聞"]
    it "strips only the trailing publisher, keeping earlier \" - \" in the headline" $
      titles "<item><title>A - B - 日本経済新聞</title><source url=\"x\">日本経済新聞</source></item>"
        `shouldBe` ["A - B"]
    it "does not strip when the suffix is not the source name" $
      titles "<item><title>見出し - 別の新聞</title><source url=\"x\">日本経済新聞</source></item>"
        `shouldBe` ["見出し - 別の新聞"]

  describe "newsItems (links)" $ do
    it "pairs each title with its absolute http(s) link" $
      newsItems "<item><title>t</title><link>https://example.com/a?x=1&amp;y=2</link></item>"
        `shouldBe` [NewsItem "t" (Just "https://example.com/a?x=1&y=2")]
    it "plain http links qualify" $
      newsItems "<item><title>t</title><link>http://example.com/</link></item>"
        `shouldBe` [NewsItem "t" (Just "http://example.com/")]
    it "link order does not matter (<link> before <title>)" $
      newsItems "<item><link>https://e.com/a</link><title>t</title></item>"
        `shouldBe` [NewsItem "t" (Just "https://e.com/a")]
    it "item without a link -> Nothing" $
      newsItems "<item><title>t</title></item>" `shouldBe` [NewsItem "t" Nothing]
    it "empty <link></link> -> Nothing" $
      newsItems "<item><title>t</title><link></link></item>"
        `shouldBe` [NewsItem "t" Nothing]
    it "self-closing <link/> -> Nothing" $
      newsItems "<item><title>t</title><link/></item>"
        `shouldBe` [NewsItem "t" Nothing]
    it "relative links are rejected" $
      newsItems rss
        `shouldBe` [NewsItem "一つ目の見出し" Nothing, NewsItem "二つ目" Nothing]
    it "CDATA-wrapped links unwrap" $
      newsItems "<item><title>t</title><link><![CDATA[https://example.com/a]]></link></item>"
        `shouldBe` [NewsItem "t" (Just "https://example.com/a")]
    it "links with whitespace are rejected" $
      newsItems "<item><title>t</title><link>https://example.com/a b</link></item>"
        `shouldBe` [NewsItem "t" Nothing]
    it "control characters are stripped from links, defusing OSC 8 breakout" $
      newsItems "<item><title>t</title><link>https://example.com/\ESC]8;;evil\a</link></item>"
        `shouldBe` [NewsItem "t" (Just "https://example.com/]8;;evil")]
    it "absurdly long links are rejected" $ do
      let long = "https://example.com/" <> T.replicate 3000 "x"
      newsItems ("<item><title>t</title><link>" <> long <> "</link></item>")
        `shouldBe` [NewsItem "t" Nothing]
