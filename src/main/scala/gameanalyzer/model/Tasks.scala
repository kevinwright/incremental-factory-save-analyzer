package gameanalyzer.model

case class Tasks(
    taskQueue: Seq[QueuedTask],
    isPaused: Boolean,
    droneTaskQueueSettings: DroneTaskQueueSettings
)
