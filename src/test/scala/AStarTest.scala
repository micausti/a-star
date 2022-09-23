import Astar2.{Coordinates, Node, fIsSmallerThanListF}

import scala.collection.mutable

object AStarTest extends App {
//TODO add in testing framework and fix tests

  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val neighbour1 = Node(Coordinates(5,4),None, 10)
  val neighbour2 = Node(Coordinates(0,0),None, 10)
  val neighbour3 = Node(Coordinates(0,1),None, 1)
  val openList = mutable.PriorityQueue(neighbour1, neighbour2, neighbour3)

  val firstNode = Node(Coordinates(5,4),None, 10)
  val secondNode = Node(Coordinates(3,3),None, 1)
  val closedList = List(firstNode, secondNode)


  //OpenListCheck
  val inOpenList = Node(Coordinates(0,0), None, 1)
  val openListCheckResult = Astar2.openListCheck(inOpenList, openList)
  println(openListCheckResult)

  //ClosedListCheck
  val isInClosedList = Node(Coordinates(5, 4), None, 1)
  val closedListCheckResult = Astar2.closedListCheck(isInClosedList, closedList)
  println(closedListCheckResult)

  //Add to OpenList Check
  val shouldBeAdded = Node(Coordinates(10, 10), None, 1) //not in either list
  val addToOpenListCheck = Astar2.shouldAddNode(shouldBeAdded, openList, closedList)
  println(addToOpenListCheck)

  val shouldNotBeAdded = Node(Coordinates(5, 4), None, 10) //already in open List and doesn't have a better f
  val addToOpenListFail = Astar2.shouldAddNode(shouldNotBeAdded, openList, closedList)
  println(addToOpenListFail)

  val shouldNotBeAdded2 = Node(Coordinates(3, 3), None, 10) //already in closed List and doesn't have a better f
  val addToOpenListFail2 = Astar2.shouldAddNode(shouldNotBeAdded, openList, closedList)
  println(addToOpenListFail2)


  //Moving nodes from open to closed list
  val goal = Node(Coordinates(5, 5), None, 1)
  val openList3 = mutable.PriorityQueue(firstNode, secondNode)
  val closedList3 = List(Node(Coordinates(0, 0), None, 1))
  val moveNodes = Astar2.moveNodesToClosedList(goal, openList3, closedList3)
  println(moveNodes)

  //Moving nodes from open to closed list when priority queue is emtpy
  val goal2 = Node(Coordinates(5, 5), None, 1)
  val openList4 = mutable.PriorityQueue()
  val closedList4 = List(Node(Coordinates(0, 0), None, 1))
  val moveNodes2 = Astar2.moveNodesToClosedList(goal, openList4, closedList4)
  println(moveNodes2)
}
