package com.pronvis.jetlore_task

import scala.xml.Elem

trait MarkTypeFormatter[A] {
  def format(markType: A, content: String): Elem
}

object MarkTypeFormatter {

  private def implicitFormat[A: MarkTypeFormatter](markType: A, content: String): Elem = {
    implicitly[MarkTypeFormatter[A]].format(markType, content)
  }

  implicit class FormatterExtensions[A: MarkTypeFormatter](val toFormat: A) {
    def format(content: String): Elem = implicitFormat(toFormat, content)
  }

  implicit val entityFormatter = {
    new MarkTypeFormatter[MarkType.Entity.type] {
      def format(markType: MarkType.Entity.type, content: String): Elem = <strong>{content}</strong>
    }
  }

  implicit val twitterUsernameFormatter = {
    new MarkTypeFormatter[MarkType.TwitterUsername.type] {
      def format(markType: MarkType.TwitterUsername.type, content: String): Elem = {
        <a href={s"http://twitter.com/$content"}>{content}</a>
      }
    }
  }

  implicit val linkFormatter = {
    new MarkTypeFormatter[MarkType.Link.type] {
      def format(markType: MarkType.Link.type, content: String): Elem = <a href={content}>{content}</a>
    }
  }

  implicit val markTypeFormatter = {
    new MarkTypeFormatter[MarkType] {
      def format(markType: MarkType, content: String): Elem = markType match {
        case entity @ MarkType.Entity                   => entity.format(content)
        case twitterUsername @ MarkType.TwitterUsername => twitterUsername.format(content)
        case link @ MarkType.Link                       => link.format(content)
      }
    }
  }

}
