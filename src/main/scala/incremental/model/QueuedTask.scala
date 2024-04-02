package incremental.model

import com.github.plokhotnyuk.jsoniter_scala.macros.named

case class QueuedTask(
    id: BigDecimal,
    name: String,
    @named("type") taskType: String,
    params: Seq[String],
    failureMessage: Option[String]
)
