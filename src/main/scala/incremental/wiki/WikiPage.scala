package incremental.wiki

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object WikiPage {
  case class Latest(
      id: Int,
      timestamp: String
  )

  case class License(
      url: String,
      title: String
  )

  given JsonValueCodec[WikiPage] = JsonCodecMaker.make
}

import WikiPage.*

case class WikiPage(
    title: String,
    content_model: String,
    source: String,
    license: License = License("", ""),
    id: Option[Int] = None,
    key: Option[String] = None,
    latest: Option[Latest] = None,
    comment: Option[String] = None,
    token: Option[String] = None
)
