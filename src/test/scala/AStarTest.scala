import Astar2.{Coordinates, Node, fIsSmallerThanListF, openListCheck, shouldAddNodeToOpenList}
import Tests._

import scala.collection.mutable

class AStarTest extends munit.FunSuite {
  def openListCheckTest(name: String, node: Node, openList: mutable.PriorityQueue[Node], expected: Option[Node]) =
    test(name) {
      assertEquals(Astar2.openListCheck(node, openList), expected)
    }

  def closedListCheckTest(name: String, node: Node, closedList: List[Node], expected: Option[Node]) =
    test(name) {
      assertEquals(Astar2.closedListCheck(node, closedList), expected)
    }

  def shouldAddNodeTest(name: String, node: Node, openList: mutable.PriorityQueue[Node], closedList: List[Node], expected: Boolean) =
    test(name) {
      assertEquals(Astar2.shouldAddNodeToOpenList(node, openList, closedList), expected)
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

  shouldAddNodeTest("should return false if it doesn't pass the openListCheck", existsInOpenListWithLargerF, openList, closedList, false)
  shouldAddNodeTest("should return false if it doesn't pass the closedListCheck", existsInClosedListWithLargerF, openList, closedList, false)
  shouldAddNodeTest("should return true if it passes both checks", existsInClosedListWithSmallerF, openList, closedList, true)

  test("make sure we get the right neighbours") {
    val node = Node(Coordinates(2, 2), None, 0.0, 0.0)
    val goal = Node(Coordinates(2, 2), None, 0.0, 0.0)
    val expected = List(
      Node(Coordinates(1, 1), Some(node), 1.4142135623730951, 0.0),
      Node(Coordinates(2, 1), Some(node), 1.0, 0.0),
      Node(Coordinates(3, 1), Some(node), 1.4142135623730951, 0.0),
      Node(Coordinates(1, 2), Some(node), 1.0, 0.0),
      Node(Coordinates(3, 2), Some(node), 1.0, 0.0),
      Node(Coordinates(1, 3), Some(node), 1.4142135623730951, 0.0),
      Node(Coordinates(2, 3), Some(node), 1.0, 0.0),
      Node(Coordinates(3, 3), Some(node), 1.4142135623730951, 0.0)
    )
    assertEquals(Astar2.getNeighbours(node, goal), expected)
  }

  test("make sure we calculate g and h correctly for neighbours") {
    val firstNode  = Node(Coordinates(0, 0), None, 0.0, 0.0)
    val secondNode = Node(Coordinates(3, 3), Some(firstNode), 0.0, 0.0)
    val thirdNode  = Node(Coordinates(5, 5), Some(secondNode), 0.0, 0.0)
    val goalNode   = Node(Coordinates(6, 6), None, 0.0, 0.0)
    val expected = List(
      Node(Coordinates(4, 4), Some(thirdNode), 2.8284271247461903, 7.0710678118654755),
      Node(Coordinates(5, 4), Some(thirdNode), 2.23606797749979, 7.0710678118654755),
      Node(Coordinates(6, 4), Some(thirdNode), 2.0, 7.0710678118654755),
      Node(Coordinates(4, 5), Some(thirdNode), 2.23606797749979, 7.0710678118654755),
      Node(Coordinates(6, 5), Some(thirdNode), 1.0, 7.0710678118654755),
      Node(Coordinates(4, 6), Some(thirdNode), 2.0, 7.0710678118654755),
      Node(Coordinates(5, 6), Some(thirdNode), 1.0, 7.0710678118654755),
      Node(Coordinates(6, 6), Some(thirdNode), 0.0, 7.0710678118654755)
    )
    assertEquals(Astar2.getNeighbours(thirdNode, goalNode), expected)
  }

  test("calculate gscore correctly when there are multiple parent nodes") {
    val firstNode  = Node(Coordinates(0, 0), None, 0.0, 0.0)
    val secondNode = Node(Coordinates(3, 3), Some(firstNode), 0.0, 0.0)
    val thirdNode  = Node(Coordinates(5, 5), Some(secondNode), 0.0, 0.0)
    val gscore     = Astar2.g(thirdNode, 0.0)
    val expected   = 7.0710678118654755
    assertEquals(gscore, expected)
  }

  test("calculate gscore correctly when there is no parent node") {
    val firstNode = Node(Coordinates(0, 0), None, 0.0, 0.0)
    val gscore    = Astar2.g(firstNode, 0.0)
    val expected  = 0.0
    assertEquals(gscore, expected)
  }

  test("process neighbours should correctly add nodes to the open  list") {
    val startNode: Node = Node(Coordinates(5, 5), None, 0.0, 0.0)
    val listOfNeighbours: List[Node] = List(
      Node(Coordinates(4, 4), None, 2.8284271247461903, 7.0710678118654755),
      Node(Coordinates(5, 4), None, 2.23606797749979, 7.0710678118654755),
      Node(Coordinates(6, 4), None, 2.0, 7.0710678118654755),
      Node(Coordinates(4, 5), None, 2.23606797749979, 7.0710678118654755),
      Node(Coordinates(6, 5), None, 1.0, 7.0710678118654755),
      Node(Coordinates(4, 6), None, 2.0, 7.0710678118654755),
      Node(Coordinates(5, 6), None, 1.0, 7.0710678118654755),
      Node(Coordinates(6, 6), None, 0.0, 7.0710678118654755)
    )
    val openList: mutable.PriorityQueue[Node]              = mutable.PriorityQueue(startNode)
    val closedList: List[Node]                             = List.empty
    val process: (mutable.PriorityQueue[Node], List[Node]) = Astar2.processNeighbours(listOfNeighbours, openList, closedList)
    val expected: (mutable.PriorityQueue[Node], List[Node]) = (
      mutable.PriorityQueue(
        Node(Coordinates(4, 4), None, 2.8284271247461903, 7.0710678118654755),
        Node(Coordinates(5, 4), None, 2.23606797749979, 7.0710678118654755),
        Node(Coordinates(6, 4), None, 2.0, 7.0710678118654755),
        Node(Coordinates(4, 5), None, 2.23606797749979, 7.0710678118654755),
        Node(Coordinates(6, 5), None, 1.0, 7.0710678118654755),
        Node(Coordinates(4, 6), None, 2.0, 7.0710678118654755),
        Node(Coordinates(5, 6), None, 1.0, 7.0710678118654755),
        Node(Coordinates(6, 6), None, 0.0, 7.0710678118654755)
      ),
      List()
    )
    assume(process == expected)
  }
}

object Tests {
  implicit val keyOrdering = new Ordering[Node] {
    override def compare(x: Node, y: Node): Int =
      x.toString.compareTo(y.toString)
  }

  val neighbour1 = Node(Coordinates(5, 4), None, 10.0, 0.0)
  val neighbour2 = Node(Coordinates(0, 0), None, 10.0, 0.0)
  val neighbour3 = Node(Coordinates(0, 1), None, 1.0, 0.0)
  val openList   = mutable.PriorityQueue(neighbour1, neighbour2, neighbour3)

  val firstNode  = Node(Coordinates(5, 4), None, 10.0, 0.0)
  val secondNode = Node(Coordinates(3, 3), None, 1.0, 0.0)
  val closedList = List(firstNode, secondNode)

  val existsInOpenListWithSmallerF = Node(Coordinates(0, 0), None, 1.0, 0.0)
  val doesNotExistInOpenList       = Node(Coordinates(3, 3), None, 1.0, 0.0)
  val existsInOpenListWithLargerF  = Node(Coordinates(0, 0), None, 11.0, 0.0)

  val existsInClosedListWithSmallerF = Node(Coordinates(0, 0), None, 1.0, 0.0)
  val doesNotExistInClosedList       = Node(Coordinates(3, 4), None, 1.0, 0.0)
  val existsInClosedListWithLargerF  = Node(Coordinates(3, 3), None, 11.0, 0.0)
}
