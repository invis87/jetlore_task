package com.pronvis.jetlore_task

trait MarkTypeFormatter[A] {
  def format(markType: A, content: String): String
}

object MarkTypeFormatter {

  implicit class MarkTypeExtension[A: MarkTypeFormatter](val toFormat: A) {
    def format(content: String): String = implicitly[MarkTypeFormatter[A]].format(toFormat, content)
  }

  implicit val entityFormatter = {
    new MarkTypeFormatter[MarkType.Entity.type] {
      def format(markType: MarkType.Entity.type, content: String): String =
        s"<strong>$content</strong>"
    }
  }

  implicit val twitterUsernameFormatter = {
    new MarkTypeFormatter[MarkType.TwitterUsername.type] {
      def format(markType: MarkType.TwitterUsername.type, content: String): String =
        s"""<a href="http://twitter.com/$content">$content</a>"""
    }
  }

  implicit val linkFormatter = {
    new MarkTypeFormatter[MarkType.Link.type] {
      def format(markType: MarkType.Link.type, content: String): String =
        s"""<a href="$content">$content</a>"""
    }
  }

  implicit val markTypeFormatter = {
    new MarkTypeFormatter[MarkType] {
      def format(markType: MarkType, content: String): String = markType match {
        case entity @ MarkType.Entity                   => entity.format(content)
        case twitterUsername @ MarkType.TwitterUsername => twitterUsername.format(content)
        case link @ MarkType.Link                       => link.format(content)
      }
    }
  }

}
