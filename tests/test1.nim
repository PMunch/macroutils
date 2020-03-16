# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import macroutils
import macros except name

test "converter, generator, accessor, and setter":
  macro test(): untyped =
    let testTableConst = TableConstr(ExprColonExpr(Lit("hello"), Lit(100)))
    testTableConst.arguments[0] = ExprColonExpr(Lit("goodbye"), Lit(42))
    testTableConst.arguments.add ExprColonExpr(Lit("world"), Lit(4242))
    testTableConst.arguments.insert(0, ExprColonExpr("hello", 32))
    assert testTableConst == nnkTableConstr.newTree(
      nnkExprColonExpr.newTree(newLit("hello"), newLit(32)),
      nnkExprColonExpr.newTree(newLit("goodbye"), newLit(42)),
      nnkExprColonExpr.newTree(newLit("world"), newLit(4242))
      )
    let testComment = CommentStmt("Hello world")
    assert testComment.argument == newLit("Hello world")
    testComment.argument = "test"
    let verifyComment = newNimNode(nnkCommentStmt)
    verifyComment.strVal = "test"
    assert testComment == verifyComment
    let testCommand = Command("testCmd", 100)
    assert testCommand == nnkCommand.newTree(newIdentNode("testCmd"), newLit(100))
    assert testCommand.name == newIdentNode("testCmd")
    testCommand.name = "echo"
    assert testCommand.name == newIdentNode("echo")
  test()
