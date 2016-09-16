package com.pronvis.jetlore_task

case class MarkZone(start: Int, end: Int) {
  require(start != end, s"Zone can't have size 0 (start=$start, end=$end)")
  require(start < end, "Zone.start should be less then Zone.end")
}

sealed trait MarkType
object MarkType {
  case object Entity extends MarkType
  case object TwitterUsername extends MarkType
  case object Link extends MarkType
}
