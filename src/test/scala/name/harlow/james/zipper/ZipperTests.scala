package name.harlow.james.zipper.btree

import org.scalatest._

class ZipperTests extends FlatSpec {

    // the expression a * b + c / d
    val expr = Section(
        Section(
            Item("a") :: Item("*") :: Item("b") :: Nil
        ) ::
        Item("+") ::
        Section(
            Item("c") :: Item("/") :: Item("d") :: Nil
        ) ::
        Nil
    )

    // the division sign in the tree
    val loc = Location(
        Item("/"),
        Node(
            Item("c") :: Nil,
            Node(
                Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                Top,
                Nil),
            Item("d") :: Nil))

    "A Zipper" should "allow left-traversal" in {
        assert(loc.left === Location(
            Item("c"),
            Node(
                Nil,
                Node(
                    Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                    Top,
                    Nil),
                Item("/") :: Item("d") :: Nil)))
    }

    it should "allow right-traversal" in {
        assert(loc.right === Location(
            Item("d"),
            Node(
                Item("/") :: Item("c") :: Nil,
                Node(
                    Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                    Top,
                    Nil),
                Nil)))
    }

    it should "allow up-traversal" in {
        assert(loc.up === Location(
            Section(
                Item("c") :: Item("/") :: Item("d") :: Nil
            ),
            Node(
                Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                Top,
                Nil)))
    }

    it should "allow down-traversal" in {
        assert(loc.up.down === loc.left)
    }

    it should "allow left-inserts" in {
        assert(loc.insertLeft(Item("/")).insertLeft(Item("1")) === Location(
            Item("/"),
            Node(
                Item("1") :: Item("/") :: Item("c") :: Nil,
                Node(
                    Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                    Top,
                    Nil),
                Item("d") :: Nil)))
    }

    it should "allow right-inserts" in {
        assert(loc.insertRight(Item("1")).insertRight(Item("*")) === Location(
            Item("/"),
            Node(
                Item("c") :: Nil,
                Node(
                    Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                    Top,
                    Nil),
                Item("*") :: Item("1") :: Item("d") :: Nil)))
    }

    it should "allow down-inserts" in {
        assert(loc.up.insertDown(Section(Item("e") :: Item("^") :: Item("f") :: Nil)) === Location(
            Section(
                Item("e") :: Item("^") :: Item("f") :: Nil
            ),
            Node(
                Nil,
                Node(
                    Item("+") :: Section(Item("a") :: Item("*") :: Item("b") :: Nil) :: Nil,
                    Top,
                    Nil
                ),
                Item("c") :: Item("/") :: Item("d") :: Nil)))
    }
}
