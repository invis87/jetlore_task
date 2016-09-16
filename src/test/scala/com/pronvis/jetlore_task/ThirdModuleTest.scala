package com.pronvis.jetlore_task

import org.scalatest.{Matchers, WordSpec}

class ThirdModuleTest extends WordSpec with Matchers {

  "FeedFormatter" should {
    "format example feed in a right way" in {
      val exampleFeed = "Obama visited Facebook headquarters: http://bit.ly/xyz @elversatile"
      val exampleMapping = Map(
        MarkZone(14, 22) -> MarkType.Entity,
        MarkZone(0, 5) -> MarkType.Entity,
        MarkZone(56, 67) -> MarkType.TwitterUsername,
        MarkZone(37, 54) -> MarkType.Link)
      val formatResult = ThirdModule.format(exampleFeed, exampleMapping)
      val wantedResult = """<strong>Obama</strong> visited <strong>Facebook</strong> headquarters: <a href="http://bit.ly/xyz">http://bit.ly/xyz</a> @<a href="http://twitter.com/elversatile">elversatile</a>"""
      formatResult shouldEqual wantedResult
    }

    "correctly format simple feed" in {
      val feed = "put in, put out"
      val mapping = Map(
        MarkZone(0,6) -> MarkType.Entity,
        MarkZone(8,15) -> MarkType.Entity)
      val formatResult = ThirdModule.format(feed, mapping)
      val wantedResult = "<strong>put in</strong>, <strong>put out</strong>"
    }

    "throw exception if MarkZone.end is bigger then 'feed' size" in {
      val feed = "some feed content"
      val mapping = Map(
        MarkZone(2, 10) -> MarkType.Link,
        MarkZone(15, 40) -> MarkType.Entity)
      assertThrows[IllegalArgumentException](ThirdModule.format(feed, mapping))
    }
  }

  "MarkZone" should {
    "throw exception if 'start' == 'end'" in {
      assertThrows[IllegalArgumentException](MarkZone(2, 2))
    }

    "throw exception if 'start' > 'end'" in {
      assertThrows[IllegalArgumentException](MarkZone(10, 2))
    }

    "works as expected" in {
      MarkZone(5, 7) shouldEqual MarkZone(5, 7)
    }
  }

  "fillHoles method" should {
    val fakeSize = 20
    "correctly work with Seq.empty" in {
      val zones = Seq.empty[MarkZone]
      ThirdModule.fillHoles(zones, fakeSize) shouldEqual Seq(MarkZone(0, fakeSize))
    }

    "throw exception if there are crossing in Zones" in {
      val zones = Seq(
        MarkZone(0, 5),
        MarkZone(5, 9),
        MarkZone(6, 8))
      assertThrows[IllegalArgumentException](ThirdModule.fillHoles(zones, fakeSize))
    }

    "correctly fill all holes" in {
      val zones = Seq(
        MarkZone(2, 4),
        MarkZone(8, 10),
        MarkZone(15,16))

      val wantedResult = Seq(
        MarkZone(0, 2),
        MarkZone(2, 4),
        MarkZone(4, 8),
        MarkZone(8, 10),
        MarkZone(10, 15),
        MarkZone(15,16),
        MarkZone(16, fakeSize))
      ThirdModule.fillHoles(zones, fakeSize) shouldEqual wantedResult
    }
  }
}
