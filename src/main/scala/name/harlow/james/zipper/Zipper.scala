package name.harlow.james.zipper.btree

object `package` {
    type Forest[T] = List[Tree[T]]
}

sealed trait Tree[T]

case class Item[T](item: T) extends Tree[T]
case class Section[T](section: Forest[T]) extends Tree[T]

sealed trait Path[+T]
case object Top extends Path[Nothing]
case class Node[T](left: Forest[T], path: Path[T], right: Forest[T]) extends Path[T]

case class Location[T](tree: Tree[T], path: Path[T]) {

    def left = path match {
        case Top => throw new Exception("left of top")
        case Node(l::left, up, right) => Location(l, Node(left, up, tree :: right))
        case Node(Nil, up, right) => throw new Exception("left of first")
    }

    def right = path match {
        case Top => throw new Exception("right of top")
        case Node(left, up, r::right) => Location(r, Node(tree :: left, up, right))
        case Node(left, up, Nil) => throw new Exception("right of last")
    }

    def up = path match {
        case Top => throw new Exception("up of top")
        case Node(left, up, right) => Location(Section(left.reverse ::: (tree :: right)), up)
    }

    def down = tree match {
        case Item(_) => throw new Exception("down of Item")
        case Section(t1 :: trees) => Location(t1, Node(Nil, path, trees))
        case _ => throw new Exception("down of Nil")
    }

    def insertRight(r: Tree[T]) = path match {
        case Top => throw new Exception("insert of top")
        case Node(left, up, right) => Location(tree, Node(left, up, r::right))
    }

    def insertLeft(l: Tree[T]) = path match {
        case Top => throw new Exception("insert of top")
        case Node(left, up, right) => Location(tree, Node(l::left, up, right))
    }

    def insertDown(t: Tree[T]) = tree match {
        case Item(_) => throw new Exception("down of item")
        case Section(children) => Location(t, Node(Nil, path, children))
    }
}
