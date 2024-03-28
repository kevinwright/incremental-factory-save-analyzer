package gameanalyzer.wiki

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object WikiTokenQueryResponse {
  case class Query(
      tokens: WikiTokens
  )

  given JsonValueCodec[WikiTokenQueryResponse] = JsonCodecMaker.make
}

import WikiTokenQueryResponse.*

case class WikiTokenQueryResponse(
    batchcomplete: String,
    query: Query
)
