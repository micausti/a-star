import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.abs
import scala.util.{Failure, Success, Try}

object Astar2 extends App {

  case class Coordinates(x: Int, y: Int)
  case class Node(coordinates: Coordinates, parent: Option[Node], h: Double, g: Double)

  //TODO test to make sure this ordering works as expected
  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val startNode                             = Node(Coordinates(0, 0), None, 0.0, 0.0)
  val goalNode                              = Node(Coordinates(10, 10), None, 0.0, 0.0)
  val openList: mutable.PriorityQueue[Node] = mutable.PriorityQueue(startNode)
  val closedList: List[Node]                = List.empty

  @tailrec
  def moveNodesToClosedList(
    goal: Node,
    openList: mutable.PriorityQueue[Node],
    closedList: List[Node]
  ): (mutable.PriorityQueue[Node], List[Node]) =
    Try {
      openList.dequeue
    } match {
      case Success(node) =>
        println("processing first item on open list")
        if (node.coordinates == goal.coordinates) {
          val closedListAtGoal = closedList ++ List(node)
          println("reached goal")
          println(s"coordinates to reach goal ${closedListAtGoal.map(_.coordinates)}")
          (openList, closedListAtGoal)
        } else {
          println("process neighbours")
          val neighbours: List[Node] = getNeighbours(node, goal)
          println(s"neighbours ${neighbours}")
          val newListsAfterProcessingNeighbours = processNeighbours(neighbours, openList, closedList)
          moveNodesToClosedList(goal, newListsAfterProcessingNeighbours._1, closedList ++ List(node))
        }
      case Failure(err) =>
        println("nothing else to process on open list")
        (openList, closedList) //If we can't get anything off the open list to process, then we will return the closed list and be finished
    }

  @tailrec
  def processNeighbours(
    neighbours: List[Node],
    openList: mutable.PriorityQueue[Node],
    closedList: List[Node]
  ): (mutable.PriorityQueue[Node], List[Node]) =
    neighbours match {
      case Nil => (openList, closedList)
      case x :: xs =>
        println("processing list")
        if (shouldAddNodeToOpenList(x, openList, closedList)) {
          println("should add node to open list")
          (openList.addOne(x), closedList)
          println(s"open list $openList")
          println(s"closed list $closedList")
        }
        processNeighbours(xs, openList, closedList)
    }

  def nodeIsInOpenList(neighbour: Node, openList: mutable.PriorityQueue[Node]): Option[Node] = openList.find(_.coordinates == neighbour.coordinates)
  def nodeIsInClosedList(neighbour: Node, closedList: List[Node]): Option[Node]              = closedList.find(_.coordinates == neighbour.coordinates)
  def fIsSmallerThanListF(neighbour: Node, listNode: Node): Boolean =
    if (f(neighbour) < f(listNode)) {
      true
    } else false

  def shouldAddNodeToOpenList(neighbour: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node]): Boolean = {
    val node = for {
      passedOpenListCheck   <- openListCheck(neighbour, openList)     //if this returns a None, we skip this neighbour and move on
      passedClosedListCheck <- closedListCheck(neighbour, closedList) //if this returns a none, we skip this neighbour, if it returns a Some, we add that to the open list
    } yield passedClosedListCheck
    node match {
      case Some(_) => true
      case None    => false
    }
  }

  def openListCheck(neighbour: Node, openList: mutable.PriorityQueue[Node]): Option[Node] =
    nodeIsInOpenList(neighbour, openList) match {
      case None => Some(neighbour)
      case Some(existsInOpenList) =>
        if (fIsSmallerThanListF(neighbour, existsInOpenList)) {
          Some(neighbour)
        } else None
    }
  def closedListCheck(neighbour: Node, closedList: List[Node]): Option[Node] =
    nodeIsInClosedList(neighbour, closedList) match {
      case None => Some(neighbour)
      case Some(existsInClosedList) =>
        if (fIsSmallerThanListF(neighbour, existsInClosedList)) {
          Some(neighbour)
        } else None
    }

  //In this implementation we are going to get the 8 surrounding squares
  //TODO need to handle neighbours that go off the grid - should these wrap around or be ignored?
  def getNeighbours(q: Node, goal: Node): List[Node] = {
    val node1       = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y - 1), Some(q), 0.0, 0.0)
    val node2       = Node(Coordinates(q.coordinates.x, q.coordinates.y - 1), Some(q), 0.0, 0.0)
    val node3       = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y - 1), Some(q), 0.0, 0.0)
    val node4       = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y), Some(q), 0.0, 0.0)
    val node5       = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y), Some(q), 0.0, 0.0)
    val node6       = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y + 1), Some(q), 0.0, 0.0)
    val node7       = Node(Coordinates(q.coordinates.x, q.coordinates.y + 1), Some(q), 0.0, 0.0)
    val node8       = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y + 1), Some(q), 0.0, 0.0)
    val initialList = List(node1, node2, node3, node4, node5, node6, node7, node8)
    initialList.map(n => n.copy(h = h(n, goal), g = g(q, 0.0))) //gets neighbours and calculates the hscore
  }

  //the estimated movement cost to move from that given square on the grid to the final destination
  def h(n: Node, goal: Node): Double =
    Math.hypot(abs(n.coordinates.x - goal.coordinates.x), abs(n.coordinates.y - goal.coordinates.y))

  //the movement cost to move from the starting point to a given square on the grid, following the path generated to get there
  @tailrec
  def g(q: Node, accDistance: Double): Double =
    q.parent match {
      case Some(n) =>
        val distance = Math.hypot(abs(q.coordinates.x - n.coordinates.x), abs(q.coordinates.y - n.coordinates.y))
        g(n, distance + accDistance)
      case None => accDistance
    }

  def f(node: Node) = node.g + node.h

}
