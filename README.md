# compact-list

[![Hackage](https://img.shields.io/hackage/v/compact-list.svg?style=flat)](https://hackage.haskell.org/package/compact-list)
[![Build Status](https://travis-ci.org/harendra-kumar/compact-list.svg?branch=master)](https://travis-ci.org/harendra-kumar/compact-list)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/sndov45axamjt6bu?svg=true)](https://ci.appveyor.com/project/harendra-kumar/compact-list)

If you hold on to a large data structure in garbage collected (GC) memory
for relatively longer times it puts undue pressure on GC, unnecessarily
increasing the work done by GC and also increasing the duration of GC
pauses. A `CompactList` allows you to keep a large list in a Compact Region
not touched by GC, thus avoiding any GC overhead.  This is essentially like
a convenient in-memory append only file where you can write a list of
Haskell values without having to marshall or serialize them.
