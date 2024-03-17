package gameanalyzer.model

case class ActiveChallenge(
    resourceId: String,
    rewardBase: Int,
    milestones: Seq[Int],
    progress: Double,
    earnedRewards: Int
)
