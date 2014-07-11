LambdNyaa
=========

LambdNyaa is a content aggregator/processor library. It was designed to
facilitate automatic fetching and notification of new episodes from
Nyaa Torrents, but can be used for pretty much any kind of content.

Conceptually, streams of `Item`s are fetched from `Source`s such as
Nyaa Torrents or RSS feeds. These Items then pass through zero or more
pipelines consisting of `Filter`s, which have the power to discard Items,
modify them, or accept them into a `Sink`. Once accepted into a Sink,
Items are removed from the stream, and will not be accepted into another.
Sinks are essentially arbitrary I/O computations. The Sinks that ship with the
library include capabilities for downloading files and remembering which Items
have been encountered in a previous run. Examples of useful Sinks which are not
yet implemented include RSS feed creation and publishing and email
notifications.

**tl;dr it's a stream processing DSL with animu support**
