module Statusline.ConfigSpec (spec) where

import Data.List (nub)
import Data.Text qualified as T
import Statusline.Config
import Test.Hspec

feedAt :: Int -> Config -> Feed
feedAt i cfg = cfgFeeds cfg !! i

spec :: Spec
spec = do
  describe "parseConfig" $ do
    context "normal" $ do
      let full =
            parseConfig
              "{\"feeds\":[{\"name\":\"nhk\",\"label\":\"NHK: \",\"url\":\"https://nhk.example/rss\"}],\
              \\"headlineCount\":5,\
              \\"rows\":{\"git\":true,\"model\":false,\"usage\":true,\"reset\":false,\"ticker\":true},\
              \\"ttl\":{\"location\":86400,\"forecast\":10800,\"news\":1200}}"
      it "extracts feeds" $
        cfgFeeds full `shouldBe` [Feed "nhk" "NHK: " "https://nhk.example/rss"]
      it "extracts headlineCount" $ cfgHeadlineCount full `shouldBe` 5
      it "extracts rows" $ cfgRows full `shouldBe` Rows True False True False True
      it "extracts ttl" $ cfgTtl full `shouldBe` Ttl 86400 10800 1200
      it "missing label defaults to name plus separator" $
        feedAt 0 (parseConfig "{\"feeds\":[{\"name\":\"hn\",\"url\":\"https://e.com\"}]}")
          `shouldBe` Feed "hn" "hn: " "https://e.com"
      it "partial config keeps defaults for the other keys" $ do
        let cfg = parseConfig "{\"rows\":{\"reset\":false}}"
        cfgRows cfg `shouldBe` defaultRows {rowReset = False}
        cfgFeeds cfg `shouldBe` cfgFeeds defaultConfig
        cfgHeadlineCount cfg `shouldBe` cfgHeadlineCount defaultConfig
        cfgTtl cfg `shouldBe` cfgTtl defaultConfig

    context "boundary" $ do
      it "empty object -> defaults" $ parseConfig "{}" `shouldBe` defaultConfig
      it "empty input -> defaults" $ parseConfig "" `shouldBe` defaultConfig
      it "explicit empty feeds means no feeds, not the defaults" $
        cfgFeeds (parseConfig "{\"feeds\":[]}") `shouldBe` []
      it "headlineCount 0 accepted" $
        cfgHeadlineCount (parseConfig "{\"headlineCount\":0}") `shouldBe` 0
      it "ttl at the 60s floor accepted" $
        ttlNews (cfgTtl (parseConfig "{\"ttl\":{\"news\":60}}")) `shouldBe` 60

    context "abnormal" $ do
      it "malformed json -> defaults" $ parseConfig "not json" `shouldBe` defaultConfig
      it "top-level array -> defaults" $ parseConfig "[1,2]" `shouldBe` defaultConfig
      it "wrong-typed headlineCount -> default" $
        cfgHeadlineCount (parseConfig "{\"headlineCount\":\"many\"}")
          `shouldBe` cfgHeadlineCount defaultConfig
      it "wrong-typed rows -> defaults" $
        cfgRows (parseConfig "{\"rows\":3}") `shouldBe` defaultRows
      it "wrong-typed feeds -> defaults" $
        cfgFeeds (parseConfig "{\"feeds\":\"x\"}") `shouldBe` cfgFeeds defaultConfig
      it "feed missing url dropped, valid sibling survives" $
        cfgFeeds
          (parseConfig "{\"feeds\":[{\"name\":\"a\"},{\"name\":\"b\",\"url\":\"https://e.com\"}]}")
          `shouldBe` [Feed "b" "b: " "https://e.com"]
      it "feed with empty name dropped" $
        cfgFeeds (parseConfig "{\"feeds\":[{\"name\":\"\",\"url\":\"https://e.com\"}]}")
          `shouldBe` []
      it "feed with non-string name dropped" $
        cfgFeeds (parseConfig "{\"feeds\":[{\"name\":3,\"url\":\"https://e.com\"}]}")
          `shouldBe` []
      it "fractional headlineCount rejected" $
        cfgHeadlineCount (parseConfig "{\"headlineCount\":2.5}")
          `shouldBe` cfgHeadlineCount defaultConfig
      it "fractional ttl rejected" $
        ttlNews (cfgTtl (parseConfig "{\"ttl\":{\"news\":90.5}}"))
          `shouldBe` ttlNews (cfgTtl defaultConfig)

    context "extreme" $ do
      it "huge headlineCount clamped to 20" $
        cfgHeadlineCount (parseConfig "{\"headlineCount\":1000000000}") `shouldBe` 20
      it "negative headlineCount clamped to 0" $
        cfgHeadlineCount (parseConfig "{\"headlineCount\":-5}") `shouldBe` 0
      it "ttl 0 clamped to 60" $
        ttlNews (cfgTtl (parseConfig "{\"ttl\":{\"news\":0}}")) `shouldBe` 60
      it "negative ttl clamped to 60" $
        ttlForecast (cfgTtl (parseConfig "{\"ttl\":{\"forecast\":-1}}")) `shouldBe` 60

  describe "feedCacheName" $ do
    it "deterministic" $
      feedCacheName (Feed "a" "" "https://e.com")
        `shouldBe` feedCacheName (Feed "a" "" "https://e.com")
    it "same name, different urls -> distinct names" $
      feedCacheName (Feed "a" "" "https://e.com/1")
        `shouldNotBe` feedCacheName (Feed "a" "" "https://e.com/2")
    it "traversal characters sanitized away" $ do
      let prefix = "feed-" :: String
          name = feedCacheName (Feed "../../etc/passwd" "" "https://e.com")
      name `shouldSatisfy` notElem '/'
      name `shouldSatisfy` (not . elem '.' . drop (length prefix))
    it "very long name capped" $ do
      let name = feedCacheName (Feed (T.replicate 500 "a") "" "https://e.com")
      -- "feed-" + 32-char name cap + "-" + 16 hex digits
      length name `shouldSatisfy` (<= 5 + 32 + 1 + 16)
    it "distinct default feeds map to distinct entries" $ do
      let names = map feedCacheName (cfgFeeds defaultConfig)
      nub names `shouldBe` names
