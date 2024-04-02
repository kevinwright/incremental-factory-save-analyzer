package incremental.model

case class Tasks(
    taskQueue: Seq[QueuedTask],
    isPaused: Boolean,
    droneTaskQueueSettings: DroneTaskQueueSettings
)
