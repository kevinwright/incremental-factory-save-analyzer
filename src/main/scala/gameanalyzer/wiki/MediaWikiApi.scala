package gameanalyzer.wiki

import com.github.plokhotnyuk.jsoniter_scala.core.*
import sttp.client4.*
import sttp.model.Uri
import sttp.model.MediaType
import sttp.model.headers.CookieWithMeta

import scala.util.{Failure, Success, Try}

val backend = DefaultSyncBackend()
val restEndpoint = uri"https://incrementalfactory.wiki.gg/rest.php/v1"
val apiEndpoint = uri"https://incrementalfactory.wiki.gg/api.php"

extension (uri: Uri) {
  def /(part: String): Uri =
    uri.addPath(part)
}

def fromJsonAs[A: JsonValueCodec]: ResponseAs[Try[A]] =
  asByteArray.map {
    case Left(err)  => Failure(RuntimeException(err))
    case Right(buf) => Try(readFromArray(buf))
  }

def asTry: ResponseAs[Try[String]] =
  asString.map {
    case Left(err)  => Failure(RuntimeException(err))
    case Right(str) => Success(str)
  }

class MediaWikiApi private (
    val tokens: WikiTokens,
    requestor: PartialRequest[Either[String, String]]
) {
  def getPage(title: String): Try[WikiPage] =
    requestor
      .get(restEndpoint / "page" / title)
      .response(fromJsonAs[WikiPage])
      .send(backend)
      .body

  def createPage(content: WikiPage): Try[WikiPage] =
    requestor
      .body(writeToString(content))
      .post(restEndpoint / "page")
      .response(fromJsonAs[WikiPage])
      .send(backend)
      .body

  def updatePage(content: WikiPage): Try[WikiPage] =
    requestor
      .body(writeToString(content))
      .put(restEndpoint / "page" / content.title)
      .response(fromJsonAs[WikiPage])
      .send(backend)
      .body

  def upsertPage(title: String, newContent: String): Try[WikiPage] = {
    getPage(title) match {
      case Success(existing) =>
        val updatedContent = existing.copy(
          source = newContent,
          comment = Some("Updated by a bot"),
          token = Some(tokens.csrftoken)
        )
        updatePage(updatedContent)
      case Failure(_) =>
        createPage(
          WikiPage(
            title = title,
            content_model = "wikitext",
            source = newContent,
            comment = Some("Updated by a bot"),
            token = Some(tokens.csrftoken)
          )
        )
    }
  }

}

object MediaWikiApi {

  private def queryTokens(
      requestor: PartialRequest[Either[String, String]]
  ): Try[(WikiTokens, Seq[CookieWithMeta])] = {

    val rawResponse = requestor
      .get(
        apiEndpoint.addParams(
          ("action", "query"),
          ("format", "json"),
          ("meta", "tokens"),
          ("type", "*")
        )
      )
      .response(fromJsonAs[WikiTokenQueryResponse])
      .send(backend)

    for {
      body <- rawResponse.body
      tokens = body.query.tokens
      cookies <- Try(rawResponse.unsafeCookies)
    } yield (tokens, cookies)
  }

  private def performLogin(
      requestor: PartialRequest[Either[String, String]],
      credentials: WikiCredentials,
      loginToken: String
  ): Try[Seq[CookieWithMeta]] = {
    val rawResponse = requestor
      .body(
        ("lgname", credentials.wikiUser),
        ("lgpassword", credentials.wikiPass),
        ("lgtoken", loginToken)
      )
      .post(
        apiEndpoint.addParams(
          ("action", "login"),
          ("format", "json")
        )
      )
      .response(asTry)
      .send(backend)

    rawResponse.body.flatMap(_ => Try(rawResponse.unsafeCookies))
  }

  def login(credentials: WikiCredentials): Try[MediaWikiApi] = {
    val requestor = (credentials.basicUser, credentials.basicPass) match {
      case (Some(u), Some(p)) => basicRequest.auth.basic(u, p)
      case _                  => basicRequest
    }

    for {
      (anonTokens, anonCookies) <- queryTokens(requestor)
      cookies <- performLogin(
        requestor.cookies(anonCookies),
        credentials,
        anonTokens.logintoken
      )
      (tokens, _) <- queryTokens(requestor.cookies(cookies))
    } yield new MediaWikiApi(
      requestor = requestor
        .cookies(cookies)
        .contentType(MediaType.ApplicationJson),
      tokens = tokens
    )
  }
}
