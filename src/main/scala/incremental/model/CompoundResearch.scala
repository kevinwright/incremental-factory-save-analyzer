package incremental.model

import cats.data.NonEmptyList
import Item.*

object CompoundResearch {
  def ordered: Array[CompoundResearch] =
    CompoundResearch.values.sortBy(_.ordinal)
}

enum CompoundResearch(
    val displayName: String,
    val values: NonEmptyList[Int],
    val costs: NonEmptyList[Set[CountedItem]]
) {
  def wikiTitle: String = s"Research $displayName"

  case energyEfficiency
      extends CompoundResearch(
        displayName = "Reduce Energy Consumption",
        values = NonEmptyList.of(
          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        ),
        costs = NonEmptyList.of(
          Set(
            redScience * 100
          ),
          Set(
            redScience * 200,
            greenScience * 100
          ),
          Set(
            redScience * 300,
            greenScience * 200
          ),
          Set(
            redScience * 400,
            greenScience * 300,
            blueScience * 100
          ),
          Set(
            redScience * 500,
            greenScience * 400,
            blueScience * 200
          ),
          Set(
            redScience * 600,
            greenScience * 500,
            blueScience * 300,
            purpleScience * 100
          ),
          Set(
            redScience * 700,
            greenScience * 600,
            blueScience * 400,
            purpleScience * 200
          ),
          Set(
            redScience * 800,
            greenScience * 700,
            blueScience * 500,
            purpleScience * 300,
            yellowScience * 100
          ),
          Set(
            redScience * 900,
            greenScience * 800,
            blueScience * 600,
            purpleScience * 400,
            yellowScience * 200
          ),
          Set(
            redScience * 1000,
            greenScience * 900,
            blueScience * 700,
            purpleScience * 500,
            yellowScience * 300,
            whiteScience * 100
          ),
          Set(
            redScience * 1200,
            greenScience * 1000,
            blueScience * 800,
            purpleScience * 600,
            yellowScience * 400,
            whiteScience * 200
          ),
          Set(
            redScience * 1400,
            greenScience * 1200,
            blueScience * 900,
            purpleScience * 700,
            yellowScience * 500,
            whiteScience * 300
          ),
          Set(
            redScience * 1700,
            greenScience * 1400,
            blueScience * 1000,
            purpleScience * 800,
            yellowScience * 600,
            whiteScience * 400
          ),
          Set(
            redScience * 2000,
            greenScience * 1700,
            blueScience * 1200,
            purpleScience * 900,
            yellowScience * 700,
            whiteScience * 500
          ),
          Set(
            redScience * 2400,
            greenScience * 2000,
            blueScience * 1400,
            purpleScience * 1000,
            yellowScience * 800,
            whiteScience * 600
          ),
          Set(
            redScience * 2900,
            greenScience * 2400,
            blueScience * 1700,
            purpleScience * 1200,
            yellowScience * 900,
            whiteScience * 700
          ),
          Set(
            redScience * 3500,
            greenScience * 2900,
            blueScience * 2000,
            purpleScience * 1400,
            yellowScience * 1000,
            whiteScience * 800
          ),
          Set(
            redScience * 4200,
            greenScience * 3500,
            blueScience * 2400,
            purpleScience * 1700,
            yellowScience * 1200,
            whiteScience * 900
          ),
          Set(
            redScience * 5000,
            greenScience * 4200,
            blueScience * 2900,
            purpleScience * 2000,
            yellowScience * 1400,
            whiteScience * 1000
          ),
          Set(
            redScience * 6000,
            greenScience * 5000,
            blueScience * 3500,
            purpleScience * 2400,
            yellowScience * 1700,
            whiteScience * 1200
          )
        )
      )

  case maxChallenges
      extends CompoundResearch(
        displayName = "Upgrade Challenge Limit",
        values = NonEmptyList.of(2, 3, 4, 5, 6, 7),
        costs = NonEmptyList.of(
          Set(
            redScience * 60,
            greenScience * 60
          ),
          Set(
            redScience * 200,
            greenScience * 200,
            blueScience * 200,
            purpleScience * 200
          ),
          Set(
            redScience * 300,
            greenScience * 300,
            blueScience * 300,
            purpleScience * 300,
            yellowScience * 300
          ),
          Set(
            redScience * 600,
            greenScience * 600,
            blueScience * 600,
            purpleScience * 600,
            yellowScience * 600,
            whiteScience * 600
          ),
          Set(
            redScience * 3000,
            greenScience * 3000,
            blueScience * 3000,
            purpleScience * 3000,
            yellowScience * 3000,
            whiteScience * 3000
          ),
          Set(
            redScience * 3000,
            greenScience * 9000,
            blueScience * 9000,
            purpleScience * 9000,
            yellowScience * 9000,
            whiteScience * 9000
          )
        )
      )

  case increaseParcelLimit
      extends CompoundResearch(
        displayName = "Increase Parcel Limit",
        values = NonEmptyList.of(
          12, 16, 20, 24, 28, 32, 36, 48, 60, 72, 84, 96, 192
        ),
        costs = NonEmptyList.of(
          Set(
            redScience * 100,
            greenScience * 100
          ),
          Set(
            redScience * 120,
            greenScience * 120
          ),
          Set(
            redScience * 180,
            greenScience * 180,
            blueScience * 180
          ),
          Set(
            redScience * 240,
            greenScience * 240,
            blueScience * 240
          ),
          Set(
            redScience * 300,
            greenScience * 300,
            blueScience * 300
          ),
          Set(
            redScience * 360,
            greenScience * 360,
            blueScience * 360,
            purpleScience * 360
          ),
          Set(
            redScience * 420,
            greenScience * 420,
            blueScience * 420,
            purpleScience * 420
          ),
          Set(
            redScience * 480,
            greenScience * 480,
            blueScience * 480,
            purpleScience * 480
          ),
          Set(
            redScience * 540,
            greenScience * 540,
            blueScience * 540,
            purpleScience * 480
          ),
          Set(
            redScience * 600,
            greenScience * 600,
            blueScience * 600,
            yellowScience * 600
          ),
          Set(
            redScience * 600,
            greenScience * 600,
            blueScience * 600,
            yellowScience * 600,
            whiteScience * 600
          ),
          Set(
            redScience * 2500,
            greenScience * 2500,
            blueScience * 2500,
            yellowScience * 2500,
            whiteScience * 2500
          ),
          Set(
            redScience * 25000,
            greenScience * 25000,
            blueScience * 25000,
            yellowScience * 25000,
            whiteScience * 25000
          )
        )
      )
}
