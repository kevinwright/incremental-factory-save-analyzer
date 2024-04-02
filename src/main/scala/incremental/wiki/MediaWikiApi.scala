package incremental.wiki

import com.github.plokhotnyuk.jsoniter_scala.core.*
import cats.effect.{IO, Resource}
import sttp.client4.*
import sttp.model.{MediaType, ResponseMetadata, StatusCode, Uri}
import sttp.model.headers.CookieWithMeta
import sttp.client4.httpclient.cats.HttpClientCatsBackend

//import scala.util.{Failure, Success, Try}

//val restEndpoint = uri"https://incrementalfactory.wiki.gg/rest.php/v1"
//val apiEndpoint = uri"https://incrementalfactory.wiki.gg/api.php"

case class WikiException(
    message: String,
    meta: ResponseMetadata
) extends RuntimeException(message) {
  def isRateLimited: Boolean = meta.code == StatusCode.TooManyRequests
}

extension (uri: Uri) {
  def /(part: String): Uri =
    uri.addPath(part)
}

def ioFromJson[A: JsonValueCodec]: ResponseAs[IO[A]] =
  asByteArray.mapWithMetadata {
    case (Left(err), meta) => IO.raiseError(WikiException(err, meta))
    case (Right(buf), _)   => IO(readFromArray(buf))
  }

def asIO: ResponseAs[IO[String]] =
  asString.mapWithMetadata {
    case (Left(err), meta) =>
      meta.code
      IO.raiseError(WikiException(err, meta))
    case (Right(str), _) => IO.pure(str)
  }

class MediaWikiApi private (
    backend: Backend[IO],
    restEndpoint: Uri,
    apiEndpoint: Uri,
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
      .recoverWith(err =>
        IO.println(err.toString) >>
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
      apiEndpoint: Uri,
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
      apiEndpoint: Uri,
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

    val uriBase = Uri
      .parse(credentials.urlBase)
      .getOrElse(sys.error(s"invalid uri: ${credentials.urlBase}"))
    val restEndpoint = uriBase.withWholePath(credentials.restPath)
    val apiEndpoint = uriBase.withWholePath(credentials.apiPath)

    println(apiEndpoint)
    println(restEndpoint)

    HttpClientCatsBackend.resource[IO]().evalMap { backend =>
      for {
        anonResponse <- queryTokens(backend, apiEndpoint, requestor)
        _ <- IO.println(anonResponse)
        (anonTokens, anonCookies) = anonResponse
        cookies <- performLogin(
          backend,
          apiEndpoint,
          requestor.cookies(anonCookies),
          credentials,
          anonTokens.logintoken
        )
        _ <- IO.println(cookies)
        loggedInResponse <- queryTokens(
          backend,
          apiEndpoint,
          requestor.cookies(cookies)
        )
        _ <- IO.println(loggedInResponse)
        (tokens, _) = loggedInResponse
      } yield new MediaWikiApi(
        backend = backend,
        restEndpoint = restEndpoint,
        apiEndpoint = apiEndpoint,
        tokens = tokens,
        requestor = requestor
          .cookies(cookies)
          .contentType(MediaType.ApplicationJson),
        logActivityToConsole = logActivityToConsole
      )
    }
  }
}
