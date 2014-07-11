-- | Example LambNyaa configuration. This configuration will search
--   NyaaTorrents for new episodes of three series every 4 hours.
--   The search results are filtered to grab the FFF releases of Rail Wars and
--   Jinsei, and the 720p HorribleSubs release of Fate/Kaleid Liner Prisma Ilya
--   2wei.
--
--   Those torrents are then marked as "already seen" so we won't accidentally
--   process them again later, downloaded into a torrent directory where they
--   will hopefully be picked up by Transmission or some other BitTorrent
--   client, and the news of their release is published into an RSS feed.
import Network.LambNyaa

main = do
  schedule def {
    cfgSources = [nyaaSearch "ilya 2wei",
                  nyaaSearch "rail wars",
                  nyaaSearch "jinsei"],
    cfgFilters = [when (not_ seenBefore <&> isEpisode <&> series)
                       (accept [seen, rss, dl])],
    cfgSchedule = Every 4 Hours
  }
  where
    -- Sinks
    rss = updateRSS "/home/someuser/www/animu.xml"
    dl = download "/home/someuser/torrents" "torrent"

    -- Series filters
    series = ilya2 <|> railwars <|> jinsei

    ilya2 = hasPrefix "fate kaleid" <&> tagged "HorribleSubs" <&> tagged "720p"
    railwars = hasPrefix "rail wars" <&> tagged "FFF"
    jinsei = hasPrefix "jinsei" <&> tagged "FFF"
