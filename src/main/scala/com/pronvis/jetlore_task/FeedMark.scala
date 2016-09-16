package com.pronvis.jetlore_task

case class FeedMark(startPosition: Int, endPosition: Int, markType: MarkType)

sealed trait MarkType
object MarkType {
  case object Entity extends MarkType
  case object TwitterUsername extends MarkType
  case object Link extends MarkType
}
