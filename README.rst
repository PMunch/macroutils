macroutils
===========
This module is meant to supplement the `macros` module in the standard
library. It adds bits and pieces that I've personally missed while writing
macros over the years.

Creating and accessing the fields of NimNodes
---------------------------------------------

One of the major things is the ability to create and
access the members of nodes more easily. With this module imported you can
create all of the useful nodes simply by dropping `nnk` from their name. So
instead of doing something like this:

.. code-block:: nim

  newStmtList(
    nnkCommand.newTree(
      newIdentNode("echo"),
      newLit("Hello world")))

You can do something like this:

.. code-block:: nim

  StmtList(
    Command(
      Ident "echo",
      Lit "Hello world"))

This just removes a lot of noise from creating trees like these. But the
procedures here are also smarter than the regular `newTree`, and literals
are automatically converted, so the above can also be written as:

.. code-block:: nim

  StmtList(Command("echo", "Hello world"))

The `Command` procedure here is aware that the first argument is often
supposed to be an `nnkIdent`, so it will automatically convert that string
into one. And the literal automatically gets turned into a `nnkStrLit`.

Another neat feature are the setters and getters for the properties of
nodes. You might have come across code like this (if you haven't, consider
yourself lucky):

.. code-block:: nim

  procImpls[0][6][2][1][1].add(
    nnkElse.newTree(
      nnkStmtList.newTree(nnkDiscardStmt.newTree(newEmptyNode()))))

This example is taken from my `protobuf` module and shows how the flat
structure of NimNodes with just a list of children nodes can be really
confusing. If you look at the generator procedures we used above you can use
the same names of the arguments there to access the nodes in those nodes. So
the above can be written as:

.. code-block:: nim

  procImpls[0].body[2].body[1].branches.add(
    Else(StmtList(DiscardStmt(Empty()))))

As you can see it is now more obvious that we're accessing the branches of
the second statement in the body which is the third child of the body of the
first statement in `procImpls`. This is still not very clear, but at least
it gives us _some_ context to what we're doing, which makes it easier to
read and debug. Thanks to the `Slice` type defined in this module it is also
possible to `add`, `insert`, index, and assign to positions in lists of
children that are offset in the node.

This alone is a useful feature when working with macros. But this module
also has some more convenience things.

Traversing the tree
-------------------

Often times when writing macros you want to manipulate only certain nodes
within the AST. Either to parse a DSL, or to modify passed in code. For this
purpose I've implemented various tree traversal procedures here. But before
they're explained I want to include the normal disclaimer that traversing
the tree isn't foolproof as it can contain template or macro calls that
won't be expanded. It's often times better to solve replacement a different
way. That being said, let's dive in.

First off we have `forNode`, it accepts a `NimNode` tree; a `NimNodeKind`,
or `set[NimNodeKind]`; an action procedure; and a max depth. There are also
templates that offer various variants of these arguments. It will traverse
the entire tree, and for each node that matches the `kind` it will replace
that node with the result of applying `action` to it. Note that it goes down
the tree first, then applies the `action` on the way up. An example that
replaces all string literals with the word "goodbye" would look like this:

.. code-block:: nim

  ourTree.forNode(nnkStrLit, (x) => Lit"goodbye")

A version of `forNode` named `forNodePos` also exists. It takes an `action`
with two arguments, the node that matched and a sequence of indices into the
tree to get to that node. This is useful if you need to know the context of
the node to change it.

As a simple helper to `forNode` there is also `replaceAll` which takes
either a kind, a set of kinds, or a node along with a node to be inserted
and replaces every node in the tree that has the same kind, is in the set of
kinds, or is the same as the node with that node.

Verifying DSL trees
-------------------

When writing DSLs it's also interesting to check if your tree is the same as
the structure you wanted. This can be done by a lot of asserts and if and
for statements. But with this module you can also use the `sameTree`
procedure that compares trees. It also accepts a list of node kinds to
ignore, if you need a placeholder for any kind, and you can specify a max
depth as well. Combine this with `forNode` and you can pretty much check any
passed in tree fairly easily. An example of what a `sameTree` check would
look like:

.. code-block:: nim

  ourTree.sameTree(quote do:
    echo "A string"
    if something:
      echo 100
  )

This would return true iff `ourTree` was a tree that contained one call that
took a `string`, and an if statement on a single `ident`, with a similar
call that took an `int`. Note that it only verifies node kinds, so it
wouldn't have to be a call to `echo`, merely a call to any `ident`. If you
wanted to verify that the two `echo` statements where actually the same you
could use `forNode` or `forNodePos` to implement that.

Building trees
--------------

One of the most welcome additions to the `macros` module has been the
`quote` macro. It is able to take a tree and interpolate symbols from your
surrounding code into it. Much like string interpolation works, just for the
AST. But it has certain limits, the most annoying of which is that it only
works for simple symbols. This module includes a `superQuote` macro that
allows you to put anything in the quotes, and rewrites it to a normal
`quote` statement that declares these as let statements. With this you can
do things like:

.. code-block:: nim

  macro testSuperQuote(input: untyped): untyped =
    let x = [newLit(100), newLit(200)]
    result = superQuote do:
      echo `$input[0].name`
      if `x[0]` == 300:
        echo "test"
      elif `x[1]` == 200:
        echo "hello world"

  testSuperQuote:
    proc someproc()

Extracting nodes from a tree
----------------------------

Creating trees is all well and good, and with `forNode` and the accessors
it's easy to get things from the tree. But to take things one step further
this module also implements what is essentially a reverse `superQuote`
statement. Since `NimNode` object can have a variable amount of children you
can also postfix your arguments with `*` to collect them into a sequence of
nodes. If the identifier exists it will assign or add to it, otherwise it
will simply create them. With this you can do something like:

.. code-block:: nim

  macro testExtract(input: untyped): untyped =
    var arguments = newSeq[NimNode](1) # Create space for body
    input.extract do:
      import `packages*`
      proc `procname`(`arguments*`): `retval` =
        `arguments[0]`
      let x: seq[`gen`]
    assert packages == @[Ident "one", Ident "two", Ident "three"]

  testExtract:
    import one, two, three
    proc someproc(arg: int, test: string): string =
      echo "Hello world"
      echo "Hello"
    let x: seq[int]


This file is automatically generated from the documentation found in
macroutils.nim. Use ``nim doc src/macroutils.nim`` to get the full documentation.
