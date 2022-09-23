import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Astar2 extends App {

  case class Coordinates(x: Int, y: Int)
  case class Node(coordinates: Coordinates, parent: Option[Node], f: Double)

  //TODO need an implicit for an ordering of Node -- not sure yet if this is really going to work
  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val startNode = Node(Coordinates(0, 0), None, 0.0)
  val goalNode  = Node(Coordinates(10, 10), None, 0.0) //TODO this shouldn't actually be a none, but just putting these here for now

  val openList: mutable.PriorityQueue[Node] = mutable.PriorityQueue(startNode)
  val closedList: List[Node]                = List.empty
  @tailrec
  def moveNodesToClosedList(
    goal: Node,
    openList: mutable.PriorityQueue[Node],
    closedList: List[Node]
  ): (mutable.PriorityQueue[Node], List[Node]) =
    tryToGetFirstElementInList(openList) match {
      case Success(node) =>
        val neighbour: Node = Node(Coordinates(0,0), None, 1) //TODO try to do this first just with one neighbour, then add processing for all neighbours
        if (neighbour.coordinates == goal.coordinates) {
          moveNodesToClosedList(goal, openList, closedList ++ List(node))
        } else {
          processNeighbours(neighbour, openList, closedList)
        }
      case Failure(e) => (openList, closedList)
    }

  def processNeighbours(neighbour: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node]): (mutable.PriorityQueue[Node], List[Node]) =
    if (shouldAddNode(neighbour, openList, closedList)) {
      (openList.addOne(neighbour), closedList)
    } else {
      (openList, closedList)
    }

  def tryToGetFirstElementInList(openList: mutable.PriorityQueue[Node]): Try[Node] = Try {
    openList.dequeue
  }

  def nodeIsInOpenList(neighbour: Node, openList: mutable.PriorityQueue[Node]): Option[Node] = openList.find(_.coordinates == neighbour.coordinates)
  def nodeIsInClosedList(neighbour: Node, closedList: List[Node]): Option[Node]              = closedList.find(_.coordinates == neighbour.coordinates)
  def fIsSmallerThanListF(neighbour: Node, listNode: Node): Boolean =
    if (neighbour.f < listNode.f) {
      true
    } else false

  def shouldAddNode(neighbour: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node]): Boolean = {
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
    //find out if neighbour is in the open list
    //if no, then return the node for further processing
    //if yes, compare the f values
    //if the node already in the open list has a lower f value, return a none
    //if the node already in the open list has a higher f value, return the node to be added to the open list
    nodeIsInOpenList(neighbour, openList) match {
      case None => Some(neighbour)
      case Some(existsInOpenList) =>
        if (fIsSmallerThanListF(neighbour, existsInOpenList)) {
          Some(neighbour)
        } else None
    }
  def closedListCheck(neighbour: Node, closedList: List[Node]): Option[Node] =
    //find out if neighbour is already in the closed list
    //if no, add it to the open list
    //if yes, find out if it has a lower f
    //if yes skip,
    //if no, add to the open list
    nodeIsInClosedList(neighbour, closedList) match {
      case None => Some(neighbour)
      case Some(existsInClosedList) =>
        if (fIsSmallerThanListF(neighbour, existsInClosedList)) {
          Some(neighbour)
        } else None
    }

  def getNeighbours(q: Node): List[Node] =
    ???
  //set parent to q
  //will need to calculate the fscore for each neighbour
  //val fScore: Double = gScore(node) + hScore(node.coordinates, goal.coordinates)

  def gScore(n: Node): Double =
    ???
  //TODO this adds together the distance between this node and q and also adds to it the g value for the start node

//  def hScore(startCoordinates: Coordinates, endCoordinates: Coordinates) = {
//    val xDist = (endCoordinates.x - startCoordinates.x).toDouble
//    val yDist = (endCoordinates.y - startCoordinates.y).toDouble
//    hypot(xDist, yDist)
//  }

//  def fScore = gScore + hScore

  def findLowestF(neighbours: List[Node])              = ???
  def compareAgainstClosedList(neighbours: List[Node]) = ???
}
