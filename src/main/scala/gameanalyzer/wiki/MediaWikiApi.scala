package gameanalyzer.wiki

import com.github.plokhotnyuk.jsoniter_scala.core.*
import cats.effect.{IO, Resource}
import sttp.client4.*
import sttp.model.Uri
import sttp.model.MediaType
import sttp.model.headers.CookieWithMeta
import sttp.client4.httpclient.cats.HttpClientCatsBackend

//import scala.util.{Failure, Success, Try}

val restEndpoint = uri"https://incrementalfactory.wiki.gg/rest.php/v1"
val apiEndpoint = uri"https://incrementalfactory.wiki.gg/api.php"

extension (uri: Uri) {
  def /(part: String): Uri =
    uri.addPath(part)
}

def ioFromJson[A: JsonValueCodec]: ResponseAs[IO[A]] =
  asByteArray.map {
    case Left(err)  => IO.raiseError(RuntimeException(err))
    case Right(buf) => IO(readFromArray(buf))
  }

def asIO: ResponseAs[IO[String]] =
  asString.map {
    case Left(err)  => IO.raiseError(RuntimeException(err))
    case Right(str) => IO.pure(str)
  }

class MediaWikiApi private (
    backend: Backend[IO],
    val tokens: WikiTokens,
    requestor: PartialRequest[Either[String, String]],
    logActivityToConsole: Boolean
) {
  def logAction(str: String): IO[Unit] =
    if logActivityToConsole then IO.println(str) else IO.unit

  def getPage(title: String): IO[WikiPage] =
    logAction(s"Getting Page $title") >> requestor
      .get(restEndpoint / "page" / title)
      .response(ioFromJson[WikiPage])
      .send(backend)
      .flatMap(_.body)

  def createPage(content: WikiPage): IO[WikiPage] =
    logAction(s"Creating Page ${content.title}") >> requestor
      .body(writeToString(content))
      .post(restEndpoint / "page")
      .response(ioFromJson[WikiPage])
      .send(backend)
      .flatMap(_.body)

  def updatePage(content: WikiPage): IO[WikiPage] =
    logAction(s"Updating Page ${content.title}") >> requestor
      .body(writeToString(content))
      .put(restEndpoint / "page" / content.title)
      .response(ioFromJson[WikiPage])
      .send(backend)
      .flatMap(_.body)

  def upsertPage(title: String, newContent: String): IO[Unit | WikiPage] = {
    getPage(title)
      .flatMap(existing =>
        if existing.source.trim == newContent.trim then
          logAction(s"Won't update $title, content is unchanged")
        else {
          logAction(s"Upserting Page $title") >>
            updatePage(
              existing.copy(
                source = newContent,
                comment = Some("Updated by a bot"),
                token = Some(tokens.csrftoken)
              )
            )
        }
      )
      .recoverWith(_ =>
        createPage(
          WikiPage(
            title = title,
            content_model = "wikitext",
            source = newContent,
            comment = Some("Updated by a bot"),
            token = Some(tokens.csrftoken)
          )
        )
      )
  }

  def upsertRedirect(title: String, target: String): IO[Unit | WikiPage] = {
    if title.equalsIgnoreCase(target)
    then logAction(s"Invalid Redirect $title => $target")
    else {
      logAction(s"Upserting Redirect $title => $target") >> upsertPage(
        title = title,
        newContent = s"#REDIRECT [[target]]"
      )
    }
  }
}

object MediaWikiApi {

  private def queryTokens(
      backend: Backend[IO],
      requestor: PartialRequest[Either[String, String]]
  ): IO[(WikiTokens, Seq[CookieWithMeta])] = {

    val responseIO = requestor
      .get(
        apiEndpoint.addParams(
          ("action", "query"),
          ("format", "json"),
          ("meta", "tokens"),
          ("type", "*")
        )
      )
      .response(ioFromJson[WikiTokenQueryResponse])
      .send(backend)

    for {
      response <- responseIO
      body <- response.body
      tokens = body.query.tokens
      cookies <- IO(response.unsafeCookies)
    } yield (tokens, cookies)
  }

  private def performLogin(
      backend: Backend[IO],
      requestor: PartialRequest[Either[String, String]],
      credentials: WikiCredentials,
      loginToken: String
  ): IO[Seq[CookieWithMeta]] = {
    val responseIO = requestor
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
      .response(asIO)
      .send(backend)
    for {
      response <- responseIO
      _ <- response.body
      cookies <- IO.delay(response.unsafeCookies)
    } yield cookies
  }

  def login(
      credentials: WikiCredentials,
      logActivityToConsole: Boolean
  ): Resource[IO, MediaWikiApi] = {
    val requestor = (credentials.basicUser, credentials.basicPass) match {
      case (Some(u), Some(p)) => basicRequest.auth.basic(u, p)
      case _                  => basicRequest
    }

    HttpClientCatsBackend.resource[IO]().evalMap { backend =>
      for {
        anonResponse <- queryTokens(backend, requestor)
        (anonTokens, anonCookies) = anonResponse
        cookies <- performLogin(
          backend,
          requestor.cookies(anonCookies),
          credentials,
          anonTokens.logintoken
        )
        loggedInResponse <- queryTokens(
          backend,
          requestor.cookies(cookies)
        )
        (tokens, _) = loggedInResponse
      } yield new MediaWikiApi(
        backend = backend,
        tokens = tokens,
        requestor = requestor
          .cookies(cookies)
          .contentType(MediaType.ApplicationJson),
        logActivityToConsole = logActivityToConsole
      )
    }
  }
}
