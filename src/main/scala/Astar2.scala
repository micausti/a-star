import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.abs
import scala.util.{Failure, Success, Try}

object Astar2 extends App {

  case class Coordinates(x: Int, y: Int)
  case class Node(coordinates: Coordinates, parent: Option[Node], f: Double, h: Double, g: Double)

  //TODO need an implicit for an ordering of Node -- not sure yet if this is really going to work
  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val startNode                             = Node(Coordinates(0, 0), None, 0.0, 0.0, 0.0)
  val goalNode                              = Node(Coordinates(10, 10), None, 0.0, 0.0, 0.0) //TODO this shouldn't actually be a none, but just putting these here for now
  val openList: mutable.PriorityQueue[Node] = mutable.PriorityQueue(startNode)
  val closedList: List[Node]                = List.empty

  def canFirstNodeBeMovedToClosedList(
    goal: Node,
    openList: mutable.PriorityQueue[Node],
    closedList: List[Node]
  ): (mutable.PriorityQueue[Node], List[Node]) =
    Try {
      openList.dequeue
    } match {
      case Success(node) => ???
      case Failure(err)  => ???
    }

  @tailrec //TODO need to get the list of neighbours to pass in here
  def moveNodesToClosedList(
    goal: Node,
    openList: mutable.PriorityQueue[Node],
    closedList: List[Node]
  ): (mutable.PriorityQueue[Node], List[Node]) =
    Try {
      openList.dequeue
    } match {
      case Success(node) =>
        if (node.coordinates == goal.coordinates) {
          moveNodesToClosedList(goal, openList, closedList ++ List(node)) //Do we need to stop all processing if we get to this point or is it ok to keep going?
        } else {
          processNeighbours(node, openList, closedList)
        }
      case Failure(err) =>
        (openList, closedList) //If we can't get anything off the open list to process, then we will return the closed list and be finished
    }

  //TODO need to change this so we pass in the list of neighbours
  def processNeighbours(neighbour: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node]): (mutable.PriorityQueue[Node], List[Node]) =
    if (shouldAddNodeToOpenList(neighbour, openList, closedList)) {
      (openList.addOne(neighbour), closedList)
    } else {
      (openList, closedList)
    }

  def nodeIsInOpenList(neighbour: Node, openList: mutable.PriorityQueue[Node]): Option[Node] = openList.find(_.coordinates == neighbour.coordinates)
  def nodeIsInClosedList(neighbour: Node, closedList: List[Node]): Option[Node]              = closedList.find(_.coordinates == neighbour.coordinates)
  def fIsSmallerThanListF(neighbour: Node, listNode: Node): Boolean =
    if (neighbour.f < listNode.f) {
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
  //TODO need to set the parent, but where should this happen?
  def getNeighbours(q: Node): List[Node] = {
    val node1 = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y - 1), None, 0.0, 0.0, 0.0)
    val node2 = Node(Coordinates(q.coordinates.x, q.coordinates.y - 1), None, 0.0, 0.0, 0.0)
    val node3 = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y - 1), None, 0.0, 0.0, 0.0)
    val node4 = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y), None, 0.0, 0.0, 0.0)
    val node5 = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y), None, 0.0, 0.0, 0.0)
    val node6 = Node(Coordinates(q.coordinates.x - 1, q.coordinates.y + 1), None, 0.0, 0.0, 0.0)
    val node7 = Node(Coordinates(q.coordinates.x, q.coordinates.y + 1), None, 0.0, 0.0, 0.0)
    val node8 = Node(Coordinates(q.coordinates.x + 1, q.coordinates.y + 1), None, 0.0, 0.0, 0.0)
    val initialList = List(node1, node2, node3, node4, node5, node6, node7, node8)
    initialList.map(n => n.copy(h = hScore(q, n))) //gets neighbours and calculates the hscore
  }

  def hScore(q: Node, n: Node): Double = { //using the euclidian distance for the heuristic
    Math.hypot(abs(q.coordinates.x - n.coordinates.x), abs(q.coordinates.y - n.coordinates.y))
  }

  def gScore = ??? //TODO the movement cost to move from the starting point to a given square on the grid, following the path generated to get there.


//  def fScore = gScore + hScore

  def findLowestF(neighbours: List[Node])              = ???
  def compareAgainstClosedList(neighbours: List[Node]) = ???
}
