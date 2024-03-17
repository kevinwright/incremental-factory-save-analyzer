package gameanalyzer.model

import com.github.plokhotnyuk.jsoniter_scala.macros.named

final case class NodeConnection(
    @named("source") sourceId: String,
    sourceHandle: Option[String],
    @named("target") targetId: String,
    targetHandle: Option[String],
    @named("type") connectionType: Option[String],
    label: Option[String],
    id: String,
    selected: Option[Boolean]
)
