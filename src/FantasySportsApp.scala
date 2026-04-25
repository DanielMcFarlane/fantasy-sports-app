//
// Fantasy Sports App
// Programming 3
// Coursework 2
//
// Created by Daniel McFarlane on 07/04/2026
//

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import scala.io.StdIn.readLine

object FantasySportsApp extends App {
  // Type alias for player data, maps name to list of weekly scores
  type PlayerData = Map[String, List[Int]]


  // -------------Application Logic -------------

  val loadResults = loadData("data.txt")

  // Handle file loading using Try
  val playerScores: PlayerData = loadResults match {
    case Success(data) => data
    case Failure(exception) =>
      println(s"Error loading file: ${exception.getMessage}")
      Map.empty
  }

  // Map menu options to their corresponding handler functions
  val actionMap = Map[Int, () => Boolean](
    1 -> handleCurrentScores,
    2 -> handleHighLow,
    3 -> handleTotalsOver500,
    4 -> handleCompare,
    5 -> handleTeamScore,
    0 -> handleQuit
  )

  // Only run if there's data loaded
  if (playerScores.nonEmpty) {
    runMenu()
  }

  // Tail recursive menu loop to avoid mutable state
  @tailrec
  def runMenu(): Unit = {
    val option = displayMenu()
    if (menuOptions(option)) runMenu()
  }


  // ------------- UI and Menu -------------

  // Display the main menu and prompt user input
  def displayMenu(): Int = {
    println("\n" + "=" * 40)
    println("       FANTASY SPORTS APPLICATION")
    println("=" * 40)
    println("1 - Display current week points")
    println("2 - Display the highest & lowest points")
    println("3 - Display players with total > 500")
    println("4 - Compare two players average scores")
    println("5 - Calculate a team score")
    println("0 - Quit")
    println("=" * 40)
    print("Select Option: ")

    readLine().trim.toIntOption.getOrElse(-1)
  }

  // Execute the selected option
  def menuOptions(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(action) => action()
      case None =>
        if (option != 0) println("Invalid option. Please try again.")
        option != 0
    }
  }

  // Hande option 1
  def handleCurrentScores(): Boolean = {
    displayCurrentScores(() => getCurrentScores(playerScores))
    true
  }

  // Handle option 2
  def handleHighLow(): Boolean = {
    displayHighLowScores(() => getHighLowScores(playerScores))
    true
  }

  // Handle option 3
  def handleTotalsOver500(): Boolean = {
    displayTotalsOver500(() => getTotalsOver500(playerScores))
    true
  }

  // Handle option 4
  def handleCompare(): Boolean = {
    displayComparedAverages(compareAverages(playerScores, _, _))
    true
  }

  // Handle options 5
  def handleTeamScore(): Boolean = {
    displayTeamScore(getTeamScore(playerScores, _, _))
    true
  }

  // Handle option 0
  def handleQuit(): Boolean = {
    println("Exiting application. Goodbye.")
    false
  }


  // ------------- Action Handlers (impure/ho) -------------

  // HO function to separate UI from score retrieval using map + mkString
  def displayCurrentScores(fetchScores: () => Map[String, Int]): Unit = {
    val scores = fetchScores()

    if (scores.isEmpty) {
      println("No player data available.")
    } else {
      println("\n          CURRENT WEEK POINTS")
      println("-" * 40)

      println(scores.toSeq.sortBy(_._1).map {
          case (player, score) =>
            s"$player: $score points"
        }
        .mkString("\n")
      )
    }
    readLine("\nPress enter to return to menu")
  }

  // HO function to separate UI from high/low score retrieval using foldLeft
  def displayHighLowScores(fetchScores: () => Map[String, (Int, Int)]): Unit = {
    val scores = fetchScores()

    if (scores.isEmpty) {
      println("No player data available.")
    } else {
      println("\n       HIGHEST AND LOWEST POINTS")
      println("-" * 40)

      val formattedOutput = scores.toSeq
        .sortBy(_._1)
        .foldLeft("") {
        case (output, (player, (high, low))) =>
          output + s"$player: High $high, Low $low\n"
      }
      println(formattedOutput)
    }
    readLine("\nPress enter to return to menu")
  }

  // HO function to seprate UI from total over 500 retrieval using for each
  def displayTotalsOver500(fetchTotals: () => Map[String, Int]): Unit = {
    val totals = fetchTotals()

    if (totals.isEmpty) {
      println("No players found with a total over 500.")
    } else {
      println("\n        PLAYERS WITH TOTAL > 500")
      println("-" * 40)

      totals.toSeq
        .sortBy(_._1)
        .foreach {
        case (player, total) =>
          println(s"$player: $total points")
      }
    }
    readLine("\nPress enter to return to menu")
  }

  // HO function to separate UI from average comparison using pattern matching
  def displayComparedAverages(compareAvg: (String, String) => Either[String, (Double, Double)]): Unit = {
    val player1 = readLine("Enter first player: ").trim.replace(" ", "_")
    val player2 = readLine("Enter second player: ").trim.replace(" ", "_")

    compareAvg(player1, player2) match {
      case Left(errorMsg) =>
        println(s"\nError: $errorMsg")

      case Right((avg1, avg2)) =>
        println("\n          AVERAGE COMPARISON")
        println("-" * 40)
        println(f"$player1 average: $avg1%.2f")
        println(f"$player2 average: $avg2%.2f")

        val resultMessage = avg1.compare(avg2) match {
          case 1 => s"$player1 has the higher average."
          case -1 => s"$player2 has the higher average."
          case 0 => "Both players have the same average."
        }
        println(s"\n$resultMessage")
    }
    readLine("\nPress enter to return to menu")
  }

  // HO function to separate UI from team score calculation
  def displayTeamScore(calcTeamScore: (List[String], Int) => (List[String], Map[String, Int], Int)): Unit = {
    val playersInput = readLine("Enter players (comma separated): ")
      .split(",")
      .map(_.trim.replace(" ", "_"))
      .filter(_.nonEmpty)
      .toList

    val weeksInput = readLine("Enter number of weeks: ")
      .toIntOption
      .filter(week => week >= 1 && week <= 20)

    if (weeksInput.isEmpty) {
      println("Invalid number of weeks.")
    } else {
      val weeks = weeksInput.get
      val (missing, playerTotals, teamTotal) = calcTeamScore(playersInput, weeks)

      if (missing.nonEmpty) {
        println(s"${missing.mkString(", ")} not found")
      }

      println("\n           CUSTOM TEAM SCORES")
      println("-" * 40)

      playerTotals.toSeq.sortBy(_._1).foreach {
        case (player, score) =>
          println(s"$player: $score points")
      }
      println(s"\nTeam total: $teamTotal points")
    }
    readLine("\nPress enter to return to menu")
  }


  // ------------- Data Operations (pure) -------------

  // Get most recent score for each player
  def getCurrentScores(data: PlayerData): Map[String, Int] =
    data.view.mapValues(_.lastOption.getOrElse(0)).toMap

  // Calculate highest and lowest score for each player
  def getHighLowScores(data: PlayerData): Map[String, (Int, Int)] =
    data.view.mapValues { scores =>
      if (scores.isEmpty) (0, 0) else (scores.max, scores.min)
    }.toMap

  // Filter players with totals over 500
  def getTotalsOver500(data: PlayerData): Map[String, Int] =
    data.view
      .mapValues(_.sum)
      .filter {
        case (_, total) =>
          total > 500
      }
      .toMap

  // Get average last 4 scores (monthly)
  def getMonthlyAverage(scores: List[Int]): Double = {
    val lastMonth = scores.takeRight(4)
    if (lastMonth.isEmpty) 0.0 else lastMonth.sum.toDouble / lastMonth.length
  }

  // Compare two averages using pattern matching
  def compareAverages(data: PlayerData, player1: String, player2: String): Either[String, (Double, Double)] = {
    (data.get(player1), data.get(player2)) match {
      case (None, None) =>
        Left(s"$player1 and $player2 not found")
      case (None, _) =>
        Left(s"$player1 not found")
      case (_, None) =>
        Left(s"$player2 not found")
      case (Some(score1), Some(score2)) =>
        Right((getMonthlyAverage(score1), getMonthlyAverage(score2)))
    }
  }

  // Calculate team score for a given week
  def getTeamScore(data: PlayerData, team: List[String], week: Int): (List[String], Map[String, Int], Int) = {
    if (team.isEmpty || week <= 0) {
      (team, Map.empty[String, Int], 0)
    } else {
      val (found, missing) = team.partition(data.contains)

      val playerTotals = found.map { player =>
        val weeksPlayed = week.min(data(player).length)
        player -> data(player).take(weeksPlayed).sum
      }.toMap

      val teamTotal = playerTotals.values.sum

      (missing, playerTotals, teamTotal)
    }
  }


  // ------------- Utilities -------------

  // Loads player scores from a text file using safe resource handling
  def loadData(filename: String): Try[PlayerData] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().map { line =>
        val parts = line.split(",").map(_.trim)
        val playerName = parts.head
        val parsedScores = parts.tail.map(_.toIntOption)

        if (parsedScores.exists(_.isEmpty)) {
          throw new IllegalArgumentException(s"Invalid data found in $playerName")
        }
        playerName -> parsedScores.flatten.toList
      }.toMap
    }
  }
}