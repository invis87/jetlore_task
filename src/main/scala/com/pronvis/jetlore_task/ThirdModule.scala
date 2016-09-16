package com.pronvis.jetlore_task

object ThirdModule {
  import MarkTypeFormatter._

  /**
    * Format feeds from First module by applying markers from Second module.
    * @param feed - First module output
    * @param markers - Second module output
    * @return
    */
  def format(feed: String, markers: Map[MarkZone, MarkType]): String = {
    val feedLength = feed.length
    val markerZones = markers.keys.toSeq
    if(markerZones.map(_.end).max > feedLength) {
      throw new IllegalArgumentException("Some MarkerZone is out of feed bounds")
    }

    val resultBuilder = StringBuilder.newBuilder
    val fullSortedMarkers = fillHoles(markerZones, feedLength)
    fullSortedMarkers.foreach(zone => {
      val content = feed.slice(zone.start, zone.end)
      resultBuilder.append(formatContent(content, zone, markers))
    })
    resultBuilder.toString()
  }

  private def formatContent(content: String, zone: MarkZone, markers: Map[MarkZone, MarkType]): String = {
    val marker = markers.get(zone)
    marker match {
      case None => content
      case Some(marker) => marker.format(content)
    }
  }

  private[jetlore_task] def fillHoles(filledZones: Seq[MarkZone], until: Int): List[MarkZone] = {
    val lastZone = filledZones.size - 1
    if(lastZone == -1) {
      return List(MarkZone(0, until))
    }

    val sortedZones = filledZones.sortBy(z => z.start)
    val accumInitState = if(sortedZones(lastZone).end == until) {
      List.empty[MarkZone]
    } else {
      List(MarkZone(sortedZones(lastZone).end, until))
    }

    val result = sortedZones.foldRight(accumInitState) { case (prevZone, accum) =>
      lazy val accumHead = accum.head
      if(accum.nonEmpty && accumHead.start != prevZone.end) {
        prevZone :: MarkZone(prevZone.end, accumHead.start) :: accum
      } else {
        prevZone :: accum
      }
    }

    if(result.head.start == 0) {
      result
    } else {
      MarkZone(0, result.head.start) :: result
    }
  }

}
