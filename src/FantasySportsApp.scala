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
  // Declare a type alias map for player data
  type PlayerData = Map[String, List[Int]]


  // -------------Application Logic -------------

  val loadResults = loadData("data.txt")

  // Handle errors gracefully when loading data using pattern matching on the Try result
  val playerScores : PlayerData = loadResults match {
    case Success(data) => data
    case Failure(_) =>
      println("Error loading file.")
      Map.empty
  }

  // Map menu options to their corresponding handler functions using an immutable map
  val actionMap = Map[Int, () => Boolean](
    1 -> handleCurrentScores,
    2 -> handleHighLow,
    3 -> handleTotalsOver500,
    4 -> handleCompare,
    5 -> handleTeamScore,
    0 -> handleQuit
  )

  // Only run if there's data
  if (playerScores.nonEmpty) {
    runMenu()
  }

  // Tail recursion to keep showing the menu until the user chooses to quit without using mutable loops
  @tailrec
  def runMenu(): Unit = {
    val option = displayMenu()
    if (menuOptions(option)) runMenu()
  }


  // ------------- UI and Menu -------------

  // Displays the main menu and prompts the user for an option
  def displayMenu() : Int = {
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

  // Executes the selected menu option and returns whether to continue showing the menu using pattern matching
  def menuOptions(option : Int) : Boolean = {
    actionMap.get(option) match {
      case Some(action) => action()
      case None =>
        if (option != 0) println("Invalid option. Please try again.")
        option != 0
    }
  }

  // Hande option 1
  def handleCurrentScores() : Boolean = {
    displayCurrentScores(() => getCurrentScores(playerScores))
    true
  }

  // Handle option 2
  def handleHighLow() : Boolean = {
    displayHighLowScores(() => getHighLowScores(playerScores))
    true
  }

  // Handle option 3
  def handleTotalsOver500() : Boolean = {
    displayTotalsOver500(() => getTotalsOver500(playerScores))
    true
  }

  // Handle option 4
  def handleCompare() : Boolean = {
    displayComparedAverages(compareAverages(playerScores, _, _))
    true
  }

  // Handle options 5
  def handleTeamScore() : Boolean = {
    displayTeamScore(getTeamScore(playerScores, _, _))
    true
  }

  // Handle option 0
  def handleQuit() : Boolean = {
    println("Exiting application. Goodbye.")
    false
  }


  // ------------- Action Handlers (impure/ho) -------------

  // HO func to display the current score for each player by accepting a pure fetch function
  def displayCurrentScores(fetchScores : () => Map[String, Int]) : Unit = {
    val scores = fetchScores()

    if (scores.isEmpty) {
      println("No player data available.")
    } else {
      println("\n          CURRENT WEEK POINTS")
      println("-" * 40)

      // Sort the scores by player name and display them
      scores.toSeq.sortBy(_._1).foreach {
        case (player, score) =>
          println(s"$player: $score points")
      }
    }
    readLine("\nPress enter to return to menu")
  }

  // HO func to display the highest and lowest scores for each player
  def displayHighLowScores(fetchScores : () => Map[String, (Int, Int)]) : Unit = {
    val scores = fetchScores()

    if (scores.isEmpty) {
      println("No player data available.")
    } else {
      println("\n       HIGHEST AND LOWEST POINTS")
      println("-" * 40)

      // Sort the scores by player name and display them
      scores.toSeq.sortBy(_._1).foreach {
        case (player, (high, low)) =>
          println(s"$player: High $high, Low $low")
      }
    }
    readLine("\nPress enter to return to menu")
  }

  // HO funct to display the total scores for players whose totals exceed 500 points
  def displayTotalsOver500(fetchTotals : () => Map[String, Int]) : Unit = {
    val totals = fetchTotals()

    if (totals.isEmpty) {
      println("No players found with a total over 500.")
    } else {
      println("\n        PLAYERS WITH TOTAL > 500")
      println("-" * 40)

      // Sort the totals by player name and display them
      totals.toSeq.sortBy(_._1).foreach {
        case (player, total) =>
          println(s"$player: $total points")
      }
    }
    readLine("\nPress enter to return to menu")
  }

  // HO func to display the average scores for two players over the last month and identify who has the higher average
  def displayComparedAverages(compareAvg : (String, String) => Either[String, (Double, Double)]) : Unit = {
    val player1 = readLine("Enter first player: ").trim.replace(" ", "_")
    val player2 = readLine("Enter second player: ").trim.replace(" ", "_")

    compareAvg(player1, player2) match {
      // Guard clause for missing players
      case Left(errorMsg) =>
        println(s"\nError: $errorMsg")

      // Use the tuple to display the averages if both players are found
      case Right((avg1, avg2)) =>
        println("\n          AVERAGE COMPARISON")
        println("-" * 40)
        println(f"$player1 average: $avg1%.2f")
        println(f"$player2 average: $avg2%.2f")

        // Compare the averages and determine the result message
        val resultMessage = avg1.compare(avg2) match {
          case 1 => s"$player1 has the higher average."
          case -1 => s"$player2 has the higher average."
          case 0 => "Both players have the same average."
        }
        println(s"\n$resultMessage")
    }
    readLine("\nPress enter to return to menu")
  }

  // HO func to display the total score for a team and identify any missing players
  def displayTeamScore(calcTeamScore : (List[String], Int) => (List[String], Map[String, Int], Int)) : Unit = {
    val playersInput = readLine("Enter players (comma separated): ")
      .split(",")
      .map(_.trim.replace(" ", "_"))
      .filter(_.nonEmpty)
      .toList

    val weeksInput = readLine("Enter number of weeks: ")
      .toIntOption
      .filter(week => week >= 1 && week <= 20)

    // Validate the number of weeks input before proceeding with the team score calculation
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

      // Sort the totals by player name display them
      playerTotals.toSeq.sortBy(_._1).foreach {
        case (player, score) =>
          println(s"$player: $score points")
      }
      println(s"\nTeam total: $teamTotal points")
    }
    readLine("\nPress enter to return to menu")
  }


  // ------------- Data Operations (pure) -------------

  // Pure func to retrieve the most recent score for each player
  def getCurrentScores(data : PlayerData) : Map[String, Int] =
    data.view.mapValues(_.lastOption.getOrElse(0)).toMap

  // Pure func to calculate the highest and lowest recorded scores for each player
  def getHighLowScores(data : PlayerData) : Map[String, (Int, Int)] =
    data.view.mapValues { scores =>
      if (scores.isEmpty) (0, 0) else (scores.max, scores.min)
    }.toMap

  // Pure func to identify players whose total scores exceed 500 points
  def getTotalsOver500(data : PlayerData) : Map[String, Int] =
    data.view
      .mapValues(_.sum)
      .filter {
        case (_, total) =>
          total > 500
      }
      .toMap

  // Pure helper to calculate the average for the last 4 scores (monthly)
  def getMonthlyAverage(scores : List[Int]) : Double = {
    val lastMonth = scores.takeRight(4)
    if (lastMonth.isEmpty) 0.0 else lastMonth.sum.toDouble / lastMonth.length
  }

  // Pure func to compare the average scores of two players for the last month using pattern matching for errors
  def compareAverages(data : PlayerData, player1 : String, player2 : String) : Either[String, (Double, Double)] = {
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

  // Pure func to get the total score for a team and finds any missing players
  def getTeamScore(data : PlayerData, team : List[String], week : Int) : (List[String], Map[String, Int], Int) = {
    // Ensure there's a team and a valid week number before proceeding
    if (team.isEmpty || week <= 0) {
      (team, Map.empty[String, Int], 0)
    } else {
      // Split the team into players there's data for and players there's not
      val (found, missing) = team.partition(data.contains)

      // Add up the scores for each found player up to the requested week
      val playerTotals = found.map { player =>
        val weeksPlayed = week.min(data(player).length)
        player -> data(player).take(weeksPlayed).sum
      }.toMap

      // Add all the individual scores together to get the team total
      val teamTotal = playerTotals.values.sum

      (missing, playerTotals, teamTotal)
    }
  }


  // ------------- Utilities -------------

  // Loads player scores from a text file with functional error handling.
  def loadData(filename : String) : Try[PlayerData] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().map { line =>
        val parts = line.split(",").map(_.trim)
        parts.head -> parts.tail.flatMap(_.toIntOption).toList
      }.toMap
    }
  }
}