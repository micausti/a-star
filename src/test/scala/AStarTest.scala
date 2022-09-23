import Astar2.{Coordinates, Node, fIsSmallerThanListF, openListCheck, shouldAddNodeToOpenList}

import scala.collection.mutable

class AStarTest extends munit.FunSuite {

  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val neighbour1 = Node(Coordinates(5, 4), None, 10)
  val neighbour2 = Node(Coordinates(0, 0), None, 10)
  val neighbour3 = Node(Coordinates(0, 1), None, 1)
  val openList   = mutable.PriorityQueue(neighbour1, neighbour2, neighbour3)

  val firstNode  = Node(Coordinates(5, 4), None, 10)
  val secondNode = Node(Coordinates(3, 3), None, 1)
  val closedList = List(firstNode, secondNode)

  val existsInOpenListWithSmallerF = Node(Coordinates(0, 0), None, 1)
  val doesNotExistInOpenList       = Node(Coordinates(3, 3), None, 1)
  val existsInOpenListWithLargerF  = Node(Coordinates(0, 0), None, 11)

  val existsInClosedListWithSmallerF = Node(Coordinates(0, 0), None, 1)
  val doesNotExistInClosedList       = Node(Coordinates(3, 4), None, 1)
  val existsInClosedListWithLargerF  = Node(Coordinates(3, 3), None, 11)

  def openListCheckTest(name: String, node: Node, openList: mutable.PriorityQueue[Node], expected: Option[Node]) =
    test(name) {
      assertEquals(Astar2.openListCheck(node, openList), expected)
    }

  openListCheckTest(
    "should add a node to the open list that already exists in the open list but has a smaller f value than the one already in there",
    existsInOpenListWithSmallerF,
    openList,
    expected = Some(existsInOpenListWithSmallerF)
  )

  openListCheckTest(
    "should add a node to the open list that does not already exist in the open list",
    doesNotExistInOpenList,
    openList,
    expected = Some(doesNotExistInOpenList)
  )

  openListCheckTest(
    "should not add a node to the open list that already exists in the open list but does not have a smaller f value than the one already in there",
    existsInOpenListWithLargerF,
    openList,
    expected = None
  )

  def closedListCheckTest(name: String, node: Node, closedList: List[Node], expected: Option[Node]) =
    test(name) {
      assertEquals(Astar2.closedListCheck(node, closedList), expected)
    }

  closedListCheckTest(
    "should add a node to the open list that already exists in the closed list but has a smaller f value than the one already in there",
    existsInClosedListWithSmallerF,
    closedList,
    expected = Some(existsInClosedListWithSmallerF)
  )

  closedListCheckTest(
    "should add a node to the open list that does not already exist in the closed list",
    doesNotExistInClosedList,
    closedList,
    expected = Some(doesNotExistInClosedList)
  )

  closedListCheckTest(
    "should not add a node to the open list that already exists in the closed list but does not have a smaller f value than the one already in there",
    existsInClosedListWithLargerF,
    closedList,
    expected = None
  )

  def shouldAddNodeTest(name: String, node: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node], expected: Boolean) =
    test(name) {
      assertEquals(Astar2.shouldAddNodeToOpenList(node, openList, closedList), expected)
    }

  shouldAddNodeTest("should return false if it doesn't pass the openListCheck", existsInOpenListWithLargerF, openList, closedList, false)
  shouldAddNodeTest("should return false if it doesn't pass the closedListCheck", existsInClosedListWithLargerF, openList, closedList, false)
  shouldAddNodeTest("should return true if it passes both checks", existsInClosedListWithSmallerF, openList, closedList, true)

}
