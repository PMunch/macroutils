## This module is meant to supplement the `macros` module in the standard
## library. It adds bits and pieces that I've personally missed while writing
## macros over the years.
##
## Creating and accessing the fields of NimNodes
## ---------------------------------------------
##
## One of the major things is the ability to create and
## access the members of nodes more easily. With this module imported you can
## create all of the useful nodes simply by dropping `nnk` from their name. So
## instead of doing something like this:
##
## .. code-block:: nim
##
##   newStmtList(
##     nnkCommand.newTree(
##       newIdentNode("echo"),
##       newLit("Hello world")))
##
## You can do something like this:
##
## .. code-block:: nim
##
##   StmtList(
##     Command(
##       Ident "echo",
##       Lit "Hello world"))
##
## This just removes a lot of noise from creating trees like these. But the
## procedures here are also smarter than the regular `newTree`, and literals
## are automatically converted, so the above can also be written as:
##
## .. code-block:: nim
##
##   StmtList(Command("echo", "Hello world"))
##
## The `Command` procedure here is aware that the first argument is often
## supposed to be an `nnkIdent`, so it will automatically convert that string
## into one. And the literal automatically gets turned into a `nnkStrLit`.
##
## Another neat feature are the setters and getters for the properties of
## nodes. You might have come across code like this (if you haven't, consider
## yourself lucky):
##
## .. code-block:: nim
##
##   procImpls[0][6][2][1][1].add(
##     nnkElse.newTree(
##       nnkStmtList.newTree(nnkDiscardStmt.newTree(newEmptyNode()))))
##
## This example is taken from my `protobuf` module and shows how the flat
## structure of NimNodes with just a list of children nodes can be really
## confusing. If you look at the generator procedures we used above you can use
## the same names of the arguments there to access the nodes in those nodes. So
## the above can be written as:
##
## .. code-block:: nim
##
##   procImpls[0].body[2].body[1].branches.add(
##     Else(StmtList(DiscardStmt(Empty()))))
##
## As you can see it is now more obvious that we're accessing the branches of
## the second statement in the body which is the third child of the body of the
## first statement in `procImpls`. This is still not very clear, but at least
## it gives us _some_ context to what we're doing, which makes it easier to
## read and debug. Thanks to the `Slice` type defined in this module it is also
## possible to `add`, `insert`, index, and assign to positions in lists of
## children that are offset in the node.
##
## This alone is a useful feature when working with macros. But this module
## also has some more convenience things.
##
## Traversing the tree
## -------------------
##
## Often times when writing macros you want to manipulate only certain nodes
## within the AST. Either to parse a DSL, or to modify passed in code. For this
## purpose I've implemented various tree traversal procedures here. But before
## they're explained I want to include the normal disclaimer that traversing
## the tree isn't foolproof as it can contain template or macro calls that
## won't be expanded. It's often times better to solve replacement a different
## way. That being said, let's dive in.
##
## First off we have `forNode`, it accepts a `NimNode` tree; a `NimNodeKind`,
## or `set[NimNodeKind]`; an action procedure; and a max depth. There are also
## templates that offer various variants of these arguments. It will traverse
## the entire tree, and for each node that matches the `kind` it will replace
## that node with the result of applying `action` to it. Note that it goes down
## the tree first, then applies the `action` on the way up. An example that
## replaces all string literals with the word "goodbye" would look like this:
##
## .. code-block:: nim
##
##   ourTree.forNode(nnkStrLit, (x) => Lit"goodbye")
##
## A version of `forNode` named `forNodePos` also exists. It takes an `action`
## with two arguments, the node that matched and a sequence of indices into the
## tree to get to that node. This is useful if you need to know the context of
## the node to change it.
##
## As a simple helper to `forNode` there is also `replaceAll` which takes
## either a kind, a set of kinds, or a node along with a node to be inserted
## and replaces every node in the tree that has the same kind, is in the set of
## kinds, or is the same as the node with that node.
##
## Verifying DSL trees
## -------------------
##
## When writing DSLs it's also interesting to check if your tree is the same as
## the structure you wanted. This can be done by a lot of asserts and if and
## for statements. But with this module you can also use the `sameTree`
## procedure that compares trees. It also accepts a list of node kinds to
## ignore, if you need a placeholder for any kind, and you can specify a max
## depth as well. Combine this with `forNode` and you can pretty much check any
## passed in tree fairly easily. An example of what a `sameTree` check would
## look like:
##
## .. code-block:: nim
##
##   ourTree.sameTree(quote do:
##     echo "A string"
##     if something:
##       echo 100
##   )
##
## This would return true iff `ourTree` was a tree that contained one call that
## took a `string`, and an if statement on a single `ident`, with a similar
## call that took an `int`. Note that it only verifies node kinds, so it
## wouldn't have to be a call to `echo`, merely a call to any `ident`. If you
## wanted to verify that the two `echo` statements where actually the same you
## could use `forNode` or `forNodePos` to implement that.
##
## Building trees
## --------------
##
## One of the most welcome additions to the `macros` module has been the
## `quote` macro. It is able to take a tree and interpolate symbols from your
## surrounding code into it. Much like string interpolation works, just for the
## AST. But it has certain limits, the most annoying of which is that it only
## works for simple symbols. This module includes a `superQuote` macro that
## allows you to put anything in the quotes, and rewrites it to a normal
## `quote` statement that declares these as let statements. With this you can
## do things like:
##
## .. code-block:: nim
##
##   macro testSuperQuote(input: untyped): untyped =
##     let x = [newLit(100), newLit(200)]
##     result = superQuote do:
##       echo `$input[0].name`
##       if `x[0]` == 300:
##         echo "test"
##       elif `x[1]` == 200:
##         echo "hello world"
##
##   testSuperQuote:
##     proc someproc()
##
## Extracting nodes from a tree
## ----------------------------
##
## Creating trees is all well and good, and with `forNode` and the accessors
## it's easy to get things from the tree. But to take things one step further
## this module also implements what is essentially a reverse `superQuote`
## statement. Since `NimNode` object can have a variable amount of children you
## can also postfix your arguments with `*` to collect them into a sequence of
## nodes. If the identifier exists it will assign or add to it, otherwise it
## will simply create them. With this you can do something like:
##
## .. code-block:: nim
##
##   macro testExtract(input: untyped): untyped =
##     var arguments = newSeq[NimNode](1) # Create space for body
##     input.extract do:
##       import `packages*`
##       proc `procname`(`arguments*`): `retval` =
##         `arguments[0]`
##       let x: seq[`gen`]
##     assert packages == @[Ident "one", Ident "two", Ident "three"]
##
##   testExtract:
##     import one, two, three
##     proc someproc(arg: int, test: string): string =
##       echo "Hello world"
##       echo "Hello"
##     let x: seq[int]
##


import macros, tables, options, sequtils


const
  AllNodeKinds* = {NimNodeKind.low..NimNodeKind.high}
  ContainerNodeKinds* = AllNodeKinds - {nnkNone, nnkEmpty, nnkNilLit,
                        nnkCharLit..nnkUInt64Lit, nnkFloatLit..nnkFloat64Lit,
                        nnkStrLit..nnkTripleStrLit}

type
  Slice* = object
    ## Object that wraps a certain slice of a NimNodes children.
    offset, length: int
    node: NimNode

template `=`*(s: Slice, values: openArray[NimNode]): untyped =
  s.node.del(s.node.start, s.node.length)
  for i, value in values:
    s.node.insert(s.node.start + i, value)

template `[]`*(s: Slice, idx: int): untyped =
  ## Access child in a NimNode slice
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx]

template `[]=`*(s: Slice, idx: int, val: untyped): untyped =
  ## Assign to child in a NimNode slice
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx] = val

template add*(s: Slice, val: untyped): untyped =
  ## Add a child into a NimNode slice
  s.node.insert(s.offset + s.length, val)

template insert*(s: Slice, pos: untyped, val: untyped): untyped =
  ## Insert a child into a NimNode slice
  assert pos in 0..s.length, "Index out of bounds"
  s.node.insert(s.offset + pos, val)

iterator items*(s: Slice): NimNode =
  ## Iterate over the nodes in a NimNode slice
  for i in 0..<s.length:
    yield s.node[s.offset + i]

template len*(s: Slice): untyped =
  ## Get length of NimNode slice
  s.length

#proc `$`*(s: Slice): string =
#  result = "["
#  for i in 0..<s.length:
#    result.add s.node[s.offset + i].repr
#    if i != s.length - 1:
#      result.add ", "
#  result.add "]"

macro massert(x, msg, node: untyped): untyped =
  quote do:
    if not `x`:
      error(`msg`, `node`)

template massert(x, node: untyped): untyped =
  let textRepr = x.repr
  massert(x, "Assertion failed: " & `textRepr`, node)

macro generate(nodes: untyped, extraFields: untyped): untyped =
  result = newStmtList()
  assert(nodes.kind == nnkStmtList)
  let
    isInitialiser = newIdentNode("isInitialiser")
    varargsNode = newIdentNode("node")
    varargsTuple = nnkTupleTy.newTree(
      nnkIdentDefs.newTree(
        newIdentNode("start"),
        newIdentNode("stop"),
        newIdentNode("int"),
        newEmptyNode()
      )
    )
  var
    fields: Table[string, seq[tuple[kind, node: NimNode]]]
    varargsCase = nnkCaseStmt.newTree(
      nnkDotExpr.newTree(varargsNode, newIdentNode("kind")))
  for node in nodes:
    massert(node.kind == nnkCall, node)
    massert(node[0].kind == nnkIdent, node[0])
    massert(node[1].kind == nnkStmtList, node[1])
    var
      positives: seq[BiggestInt]
      negatives: seq[BiggestInt]
      flexible =
        (start: -1.BiggestInt, stop: -1.BiggestInt, node: newEmptyNode())
    for arg in node[1]:
      massert(arg.kind == nnkCall, arg)
      massert(arg[0].kind == nnkBracketExpr, arg[0])
      massert(arg[0][0].kind == nnkIdent, arg[0][0])
      case arg[0][1].kind:
      of nnkIntLit:
        positives.add arg[0][1].intVal
      of nnkInfix:
        massert(arg[0][1][0].kind == nnkIdent, arg[0][1][0])
        case $arg[0][1][0]:
        of "..":
          for i in arg[0][1][1].intVal..arg[0][1][2].intVal:
            positives.add i
        of "..<":
          for i in arg[0][1][1].intVal..<arg[0][1][2].intVal:
            positives.add i
        of "..^":
          flexible =
            (start: arg[0][1][1].intVal, stop: arg[0][1][2].intVal, node: arg)
      of nnkPrefix:
        negatives.add arg[0][1][1].intVal
      else:
        massert(arg[0][1].kind in {nnkIntLit, nnkInfix}, arg[0][1])
    #positives.sort
    #negatives.sort
    for i, v in positives:
      massert(i == v, node)
    if flexible.start != -1:
      massert(positives.len == 0 or positives[^1]+1 == flexible.start, node)
      massert(negatives.len == 0 or negatives[0]+1 == flexible.stop, node)
      massert(positives.len != 0 or flexible.start == 0, node)
      massert(negatives.len != 0 or flexible.stop == 1, node)
    for i, v in negatives:
      massert(negatives.len - i == v, node)
    massert(negatives.len == 0 or flexible.start != -1, node)
    var generator = nnkProcDef.newTree(
      nnkPostfix.newTree(newIdentNode("*"), node[0]),
      newEmptyNode(), newEmptyNode(),
      nnkFormalParams.newTree(newIdentNode("NimNode")),
      newEmptyNode(), newEmptyNode(), newStmtList())
    for arg in node[1]:
      if arg[0][0].strVal != "_":
        generator[3].add nnkIdentDefs.newTree(arg[0][0], arg[1], newEmptyNode())
    let
      nodeKind = newIdentNode("nnk" & $node[0])
      argcount =
        if flexible.start == -1:
          newLit(positives.len)
        else:
          nnkInfix.newTree(newIdentNode("+"),
            newLit(positives.len + negatives.len),
            nnkDotExpr.newTree(flexible.node[0][0], newIdentNode("len")))
    if flexible.start != -1:
      varargsCase.add nnkOfBranch.newTree(
        nodeKind,
        newStmtList(
          nnkCall.newTree(newIdentNode("some"),
          nnkPar.newTree(
            nnkExprColonExpr.newTree(
              newIdentNode("start"), newLit(flexible.start.int)),
            nnkExprColonExpr.newTree(
              newIdentNode("stop"), newLit(flexible.stop.int))))))
    else:
      varargsCase.add nnkOfBranch.newTree(
        nodeKind,
        nnkCall.newTree(newIdentNode("none"), varargsTuple))
    generator[6].add quote do:
      const `isInitialiser` {.used.} = true
      result = newNimNode(`nodeKind`)
      #result.sons = newSeq[NimNode](`argcount`)
      for i in 0..<`argcount`:
        result.add newEmptyNode()
    for arg in node[1]:
      if arg[0][0].strVal == "_":
        continue
      if fields.hasKeyOrPut(arg[0][0].strVal, @[(kind: node[0], node: arg)]):
        fields[arg[0][0].strVal].add (kind: node[0], node: arg)
      if arg.len == 3:
        generator[6].add arg[2]
      else:
        # Add in some default setters
        case arg[0][1].kind:
        of nnkIntLit, nnkPrefix:
          let
            pos = arg[0][1]
            argName = arg[0][0]
          generator[6].add quote do:
            result[`pos`] = `argName`
        of nnkInfix:
          case $arg[0][1][0]:
          of "..", "..<":
            let
              argName = arg[0][0]
              theRange = arg[0][1]
              start = arg[0][1][1]
            generator[6].add quote do:
              for i in `theRange`:
                result[i] = `argName`[i-`start`]
          of "..^":
            let
              argName = arg[0][0]
              start = arg[0][1][1]
            generator[6].add quote do:
              for i in 0..`argName`.high:
                result[`start` + i] = `argName`[i]
        else: discard
    result.add generator
  for field, nodes in fields:
    let
      nameNode = newIdentNode(field)
      setterNameNode = newIdentNode(field & "=")
      x = newIdentNode("x")
      val = newIdentNode(field)
    var getter = quote do:
      template `nameNode`*(`x`: NimNode): untyped =
        case `x`.kind:
        else:
          raise newException(ValueError, "Unable to get " & `field` &
            " for NimNode of kind " & $`x`.kind)
    var setter = quote do:
      template `setterNameNode`*(`x`: NimNode, `val`: untyped): untyped =
        const `isInitialiser` {.used.} = false
        template result(): untyped {.used.} = `x`
        case `x`.kind:
        else:
          raise newException(ValueError, "Unable to set " & `field` &
            " for NimNode of kind " & $`x`.kind)
    for node in nodes:
      let
        getterBranch =
          nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
        setterBranch =
          nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
        indices = node.node[0][1]
      case indices.kind:
      of nnkIntLit, nnkPrefix:
        getterBranch.add quote do:
          `x`[`indices`]
        if node.node.len == 3:
          setterBranch.add node.node[2]
        else:
          setterBranch.add quote do:
            `x`[`indices`] = `val`
      of nnkInfix:
        case $indices[0]:
        of "..", "..<", "..^":
          let
            start = indices[1].intVal
            stop = indices[2].intVal
            length = stop - start + (if $indices[0] == "..<": 0 else: 1)
          if $indices[0] == "..^":
            getterBranch.add quote do:
              Slice(offset: `start`, length: `x`.len - `start` - `stop` + 1, node: `x`)
            if node.node.len == 3:
              setterBranch.add node.node[2]
            else:
              setterBranch.add quote do:
                `x`.del(`start`, `x`.len - `stop` - `start` + 1)
                for i, v in `val`:
                  `x`.insert(i + `start`, v)
          else:
            getterBranch.add quote do:
              Slice(offset: `start`, length: `length`, node: `x`)
            if node.node.len == 3:
              setterBranch.add node.node[2]
            else:
              setterBranch.add quote do:
                assert `val`.len == `length`, "Unable to set fixed size field to different length: " & `field` & " in node of kind " & $`x`.kind
                for i, v in `val`:
                  `x`[i + `start`] = v
      else: discard
      getter[6][0].insert getter[6][0].len - 1, getterBranch
      setter[6][2].insert setter[6][2].len - 1, setterBranch
    for extra in extraFields:
      massert(extra.kind == nnkCall, extra)
      massert(extra.len == 2, extra)
      massert(extra[0].kind == nnkIdent, extra[0])
      massert(extra[1].kind == nnkIdent, extra[1])
      if extra[1].strVal == field:
        getter[6][0].insert getter[6][0].len - 1, nnkOfBranch.newTree(newIdentNode("nnk" & extra[0].strVal), nnkCall.newTree(newIdentNode("newLit"), nnkDotExpr.newTree(x, newIdentNode("strVal"))))
        setter[6][2].insert setter[6][2].len - 1, nnkOfBranch.newTree(newIdentNode("nnk" & extra[0].strVal), nnkAsgn.newTree(nnkDotExpr.newTree(x, newIdentNode("strVal")), nameNode))
    result.add getter
    result.add setter
  varargsCase.add nnkElse.newTree(
    nnkCall.newTree(newIdentNode("none"), varargsTuple))
  result.add quote do:
    proc getVarargs(`varargsNode`: NimNode): Option[`varargsTuple`] =
      `varargsCase`
  #echo result.repr


macro createLitConverters(list: varargs[untyped]): untyped =
  result = newStmtList()
  let x = newIdentNode("x")
  for kind in list:
    result.add quote do:
      converter Lit*(`x`: `kind`): NimNode = newLit(`x`)

createLitConverters(char, int, int8, int16, int32, int64, uint, uint8, uint16,
                    uint32, uint64, bool, string, float32, float64)

#proc newLit*(x: NimNode): NimNode =
#  assert(x.kind in nnkLiterals, "Node of kind " & $x.kind & " is not a Literal")
#  x

#converter Lit*[N, T](x: array[N, T]): NimNode = newLit(`x`)
#converter Lit*[T](x: seq[T]): NimNode = newLit(`x`)
#converter Lit*[T](x: set[T]): NimNode = newLit(`x`)

proc asIdent(name: string | NimNode): NimNode =
  when name is NimNode:
    # These NimNodes can really be anything..
    #assert name.kind in {nnkIdent, nnkSym},
    #  "Node must be an identifier or a symbol, but was: " & $name.kind & "(" &
    #  name.repr & ")"
    name
  else:
    newIdentNode(name)

proc Ident*(name: string): NimNode =
  newIdentNode(name)

proc Sym*(name: static[string]): NimNode =
  bindSym(name)

proc RStrLit*(argument: string): NimNode =
  result = newNimNode(nnkRStrLit)
  result.strVal = argument

proc CommentStmt*(argument: string): NimNode =
  result = nnkCommentStmt.newTree()
  result.strVal = argument

proc BlockStmt*(body: NimNode): NimNode =
  nnkBlockStmt.newTree(newNimNode(nnkEmpty), body)

proc ContinueStmt*(): NimNode =
  newNimNode(nnkContinueStmt)

proc Empty*(): NimNode =
  newEmptyNode()

proc AsmStmt*(body: string | NimNode): NimNode =
  nnkAsmStmt.newTree(newEmptyNode(), body)

proc RaiseStmt*(): NimNode =
  nnkRaiseStmt.newTree(newEmptyNode())

generate:
  Command:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    arguments[1..^1](varargs[NimNode])

  Call:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    arguments[1..^1](varargs[NimNode])

  Infix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    left[1](NimNode)
    right[2](NimNode)

  Prefix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](NimNode)

  Postfix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](NimNode)

  ExprEqExpr:
    left[0](NimNode)
    right[1](NimNode)

  ExprColonExpr:
    left[0](NimNode)
    right[1](NimNode)

  CallStrLit:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](string | NimNode):
      result[1] = when argument is NimNode:
        case argument.kind:
        of nnkRStrLit: argument
        of nnkStrLit: RStrLit(argument.strVal)
        else:
          raise newException(ValueError,
            "Unable to convert NimNode of kind " & $argument.kind & " to nnkRStrLit")
      else:
        RStrLit(argument)

  DerefExpr:
    node[0](NimNode)

  Addr:
    node[0](NimNode)

  Cast:
    bracket[0](NimNode)
    node[1](NimNode)

  DotExpr:
    left[0](NimNode)
    right[1](NimNode)

  BracketExpr:
    node[0](NimNode)
    bracket[1](NimNode)

  Par:
    arguments[0..^1](varargs[NimNode])

  Curly:
    arguments[0..^1](varargs[NimNode])

  Bracket:
    arguments[0..^1](varargs[NimNode])

  TableConstr:
    arguments[0..^1](varargs[NimNode]):
      for i, a in arguments:
        assert a.kind == nnkExprColonExpr,
          "Unable to add non-colon expression to table constructor: " & $a.kind
        result[i] = a

  IfExpr:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to if constructor: " & $a.kind
        result[i] = a

  IfStmt:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to if constructor: " & $a.kind
        result[i] = a

  WhenStmt:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to when constructor: " & $a.kind
        result[i] = a

  ElifExpr:
    cond[0](NimNode)
    body[1](NimNode)

  ElifBranch:
    cond[0](NimNode)
    body[1](NimNode)

  ElseExpr:
    body[0](NimNode)

  Else:
    body[0](NimNode)

  Pragma:
    arguments[0..^1](varargs[NimNode])

  Asgn:
    left[0](NimNode)
    right[1](NimNode)

  StmtList:
    arguments[0..^1](varargs[NimNode])

  CaseStmt:
    cond[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to case constructor: " & $a.kind
        result[1 + i] = a

  OfBranch:
    arguments[0..^2](openarray[Nimnode])
    body[^1](NimNode)

  WhileStmt:
    cond[0](NimNode)
    body[1](NimNode)

  ForStmt:
    arguments[0..^3](openarray[NimNode])
    iter[^2](NimNode)
    body[^1](NimNode)

  TryStmt:
    body[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, branch in branches:
        assert branch.kind in {nnkExceptBranch, nnkFinally},
          "Unable to add non-except or -finally expression to try " &
          "constructor: " & $branch.kind
        result[1 + i] = branch

  ExceptBranch:
    arguments[0..^2](openarray[NimNode])
    body[^1](NimNode)

  Finally:
    body[0](NimNode)

  ReturnStmt:
    argument[0](NimNode)

  YieldStmt:
    argument[0](NimNode)

  DiscardStmt:
    argument[0](NimNode)

  BreakStmt:
    argument[0](NimNode)

  BlockStmt:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    body[1](NimNode)

  AsmStmt:
    pragmas[0](NimNode)
    body[1](string | NimNode):
      result[1] = when body is string: newLit(body) else: body

  ImportStmt:
    arguments[0..^1](varargs[NimNode])

  ImportExceptStmt:
    left[0](NimNode)
    right[1](NimNode)

  FromStmt:
    left[0](NimNode)
    right[1](NimNode)

  ExportStmt:
    argument[0](NimNode)

  ExportExceptStmt:
    left[0](NimNode)
    right[1](NimNode)

  IncludeStmt:
    arguments[0..^1](varargs[NimNode])

  VarSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident definition to var section " &
          "constructor: " & $def.kind
        result[i] = def

  LetSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident definition to let section " &
          "constructor: " & $def.kind
        result[i] = def

  ConstSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkConstDef,
          "Unable to add something not an constant definition to const " &
          "section constructor: " & $def.kind
        result[i] = def

  IdentDefs:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    typ[1](NimNode)
    body[2](NimNode)

  ConstDef:
    name[0](NimNode)
    typ[1](NimNode)
    body[2](NimNode)

  Range:
    left[0](NimNode)
    right[1](NimNode)

  AccQuoted:
    arguments[0..^1](varargs[NimNode])

  BindStmt:
    argument[0](NimNode)

  GenericParams:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident def to generic parameters " &
          "constructor: " & $def.kind
        result[i] = def

  FormalParams:
    retval[0](NimNode)
    arguments[1..^1](varargs[NimNode])

  OfInherit:
    argument[0](NimNode)

  VarTy:
    argument[0](NimNode)

  ProcDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  FuncDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  IteratorDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  ConverterDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  MethodDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  TemplateDef:
    name[0](NimNode)
    terms[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  MacroDef:
    name[0](NimNode)
    terms[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  TypeSection:
    definitions[0..^1](varargs[NimNode])

  TypeDef:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    generics[1](NimNode)
    typ[2](NimNode)

  Defer:
    body[0](NimNode)

  RaiseStmt:
    node[0](NimNode)

  MixinStmt:
    node[0](NimNode)

  TupleTy:
    definitions[0..^1](varargs[NimNode])

  PtrTy:
    node[0](NimNode)

  RefTy:
    node[0](NimNode)

  DistinctTy:
    node[0](NimNode)

  EnumTy:
    _[0](NimNode)
    definitions[1..^1](varargs[NimNode])

  TypeClassTy:
    arglist[0](NimNode)
    _[1](NimNode)
    _[2](NimNode)
    body[3](NimNode)

  Arglist:
    arguments[0..^1](varargs[NimNode])

  ProcTy:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  IteratorTy:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  StaticTy:
    node[0](NimNode)

  ConstTy:
    node[0](NimNode)

  MutableTy:
    node[0](NimNode)

  SharedTy:
    node[0](NimNode)

  StaticStmt:
    body[0](NimNode)

  UsingStmt:
    definitions[0..^1](varargs[NimNode])

  TypeOfExpr:
    node[0](NimNode)

  ObjectTy:
    pragmas[0](NimNode)
    inherits[1](NimNode)
    body[2](NimNode)

  RecList:
    arguments[0..^1](varargs[NimNode])

  RecCase:
    cond[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to case constructor: " & $a.kind
        result[1 + i] = a

  RecWhen:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to when constructor: " & $a.kind
        result[i] = a

  EnumFieldDef:
    left[0](NimNode)
    right[1](NimNode)

  ObjConstr:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    definitions[1..^1](varargs[NimNode])
do:
  Ident(name)
  Sym(name)
  RStrLit(argument)
  CommentStmt(argument)

template inOrEquals*(node: NimNode,
                    nodekind: NimNodeKind or NimNodeKinds): untyped =
  when nodekind is NimNodeKind:
    node.kind == nodekind
  else:
    node.kind in nodekind

proc forNodePos*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x: NimNode, y: seq[int]): NimNode,
              depth, maxDepth: int,
              expr: seq[int] = @[]): NimNode {.discardable.} =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is not the node itself, but
  ## rather it's position in the tree as a sequence of indexes. NOTE: This
  ## modifies the original node tree and only returns for easy chaining.
  var node = if node.inOrEquals(kind) and depth <= maxdepth:
      action(node, expr)
    else:
      node

  result = node
  if node.kind in ContainerNodeKinds and depth < maxdepth:
    for i, child in node:
      result[i] = forNodePos(child, kind, action, depth + 1, maxdepth, expr & i)

template forNodePos*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  maxdepth: int,
                  action: proc (x: NimNode, y: seq[int]): NimNode): NimNode =
  forNodePos(node, kind, action, 0, maxdepth)

template forNodePos*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  action: proc (x: NimNode, y: seq[int]): NimNode): NimNode =
  forNodePos(node, kind, action, 0, int.high)

proc forNodePos*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x: NimNode, y: seq[int]),
              depth = 0, maxDepth = int.high,
              expr: seq[int] = @[]) =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is not the node itself, but
  ## rather it's position in the tree as a sequence of indexes. This version
  ## does not mutate the underlying node.
  if node.inOrEquals(kind) and depth <= maxdepth:
    action(node, expr)

  if node.kind in ContainerNodeKinds and depth < maxdepth:
    for i, child in node:
      forNodePos(child, kind, action, depth + 1, maxdepth, expr & i)

proc forNode*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x: NimNode): NimNode,
              depth, maxDepth: int): NimNode {.discardable.} =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is the node in the tree.
  ## NOTE: This modifies the original node tree and only returns for easy
  ## chaining.

  # If it's a container, recurse into it, otherwise return it
  result = node
  if node.kind in ContainerNodeKinds and depth < maxdepth:
    for i, child in node:
      result[i] = forNode(child, kind, action, depth + 1, maxdepth)

  # Check if this node is equals, but continue checking within it if it's a
  # container
  result =
    if result.inOrEquals(kind) and depth <= maxdepth:
      action(result)
    else: result

template forNode*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  maxdepth: int,
                  action: proc (x: NimNode): NimNode): NimNode =
  forNode(node, kind, action, 0, maxdepth)

template forNode*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  action: proc (x: NimNode): NimNode): NimNode =
  forNode(node, kind, action, 0, int.high)

proc forNode*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x: NimNode),
              depth = 0, maxDepth = int.high) =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is the node in the tree.
  ## NOTE: This does not modify the original node tree and returns for easy
  ## chaining.

  # If it's a container, recurse into it, otherwise return it
  if node.kind in ContainerNodeKinds and depth < maxdepth:
    for i, child in node:
      forNode(child, kind, action, depth + 1, maxdepth)

  # Check if this node is equals, but continue checking within it if it's a
  # container
  if node.inOrEquals(kind) and depth <= maxdepth:
    action(node)

template replaceAll*(node: NimNode,
                     kind: NimNodeKind or NimNodeKinds,
                     replace: NimNode,
                     maxdepth = int.high): NimNode =
  ## Replaces all nodes of `kind` in the tree given by `node` with `replace`
  forNode(node, kind, maxdepth, proc(x: NimNode): NimNode = replace)

template replaceAll*(node: NimNode,
                     find: NimNode,
                     replace: NimNode,
                     maxdepth = int.high): NimNode =
  ## Replaces all nodes that are equal to `find` in the tree given by `node`
  ## with `replace`
  forNode(node, find.kind, maxdepth,
    proc(x: NimNode): NimNode = (if find == x: replace else: x))

proc sameTree*(node: NimNode,
               ignored: NimNodeKind or NimNodeKinds,
               comp: NimNode,
               depth = 0, maxDepth = int.high): bool =
  ## Compares two NimNode trees and verifies that the structure and all kinds
  ## are the same.
  if node.inOrEquals(ignored) or depth >= maxdepth:
    return true
  if node.kind in ContainerNodeKinds:
    result = node.len == comp.len
    for i, child in node:
      if child.inOrEquals(ignored):
        continue
      elif child.kind == comp[i].kind:
        result = result and
          sameTree(child, ignored, comp[i], depth + 1, maxdepth)
      else:
        result = false
  else:
    result = node.kind == comp.kind

template sameTree*(node: NimNode, comp: NimNode): bool =
  sameTree(node, {}.NimNodeKinds, comp)

template sameTree*(node: NimNode, maxdepth: int, comp: NimNode): bool =
  sameTree(node, {}.NimNodeKinds, comp, 0, maxdepth)

macro superQuote*(x: untyped): untyped =
  ## Converts the input to a `quote` statement, but lifts out the content of
  ## the quoted sections as variables which allows you to do things like:
  ##
  ## .. code-block:: nim
  ##   macro someMacro(input: untyped): untyped =
  ##     let x = [newLit(100), newLit(200)]
  ##     result = superQuote do:
  ##       echo `$input[0].name` # If input is a procedure, this is the name of the procedure
  ##       if `x[0]` == 300: # Grabs the first literal
  ##         echo "test"
  ##       elif `x[1]` == 200: # Grabs the second literal
  ##         echo "hello world"
  ##
  ## Note that the content of a quoted node is not parsed by Nim, so the content
  ## of the quoted node goes through `parseExpr` which might introduce some
  ## weird behaviour for complex statements.

  var
    defs = LetSection()
    body = x.forNode(nnkAccQuoted, proc(x: NimNode): NimNode =
      var str = ""
      for child in x:
        str.add $child
      let sym = genSym()
      defs.add IdentDefs(sym, Empty(), parseExpr(str))
      x.del 0, x.len
      x.add sym
    )
  result = StmtList(defs, Call("quote", body))

macro isVariable(x: typed): bool =
  newLit x.kind == nnkSym and x.symKind() == nskVar

macro extract*(ast, pattern: untyped): untyped =
  ## A reverse superQuote macro, takes an AST and a pattern with the same tree
  ## structure but where nodes can be replaced by quoted statements. The macro
  ## will then assign the node in the tree to this statement. If the statement
  ## ends with `*` it will be a sequence of `NimNode` that gets added to.
  ##
  ## If you can't replace a node in the tree with a single quoted statement
  ## because of Nims syntax, for example to grab all variable assignments in a
  ## block like this:
  ##
  ## .. code-block:: nim
  ##   var
  ##     firstVariable = 100
  ##     secondVariable = "string"
  ##
  ## You can instead add quoted statements with a single `*` in them to tell
  ## `extract` to combine these fields, like so:
  ##
  ## .. code-block:: nim
  ##   macro someMacro(input: untyped): untyped =
  ##     input.extract:
  ##       var
  ##         `fields*` = `*`
  ##    for field in fields:
  ##      echo field.treeRepr
  ##
  ##   someMacro:
  ##     var
  ##       firstVariable = 100
  ##       secondVariable = "string"
  result = StmtList()

  #echo pattern.treeRepr

  # This lifts the AccQuoted up to the highest possible level
  pattern.forNode(ContainerNodeKinds, proc (x: NimNode): NimNode =
    if x.getVarargs.isSome:
      if x.kind == nnkStmtList and x.len == 1 and x[0].kind == nnkAccQuoted:
        return x[0]
    else:
      block checking:
        var foundNode: NimNode
        for pos, node in x:
          if (node.kind == nnkAccQuoted and node.len == 1 and
            $node[0] == "*") or node.kind == nnkEmpty:
            continue
          elif node.kind == nnkAccQuoted:
            if foundNode.kind != nnkNilLit:
              break checking
            foundNode = node
          else:
            break checking
        if foundNode.kind != nnkNilLit:
          return foundNode
    return x
  )

  # TODO: Verify that pattern is the same tree structure as input

  # This finds and extracts the AccQuoted nodes
  var x: seq[tuple[node: NimNode, pos: seq[int]]]
  pattern.forNodePos(nnkAccQuoted, proc (n: NimNode, y: seq[int]): NimNode =
    var str = ""
    for child in n:
      if $child != "*":
        str.add $child
    x.add (parseExpr(str), y)
    n
  )

  #echo pattern.treeRepr
  # This performs an important check, but it doesn't work when the AST from
  # above is malformed..
  #result = StmtList(quote do:
  #  let ptrn = quote do:
  #    `pattern`
  #  assert ptrn.sameTree(nnkAccQuoted, `ast`),
  #    "Unable to run extract on different trees"
  #)

  for y in x:
    # TODO: for each vararg node, check if it has a single child with only
    # AccQuoted nodes with stars. If it does, check the kind of every child,
    # and extract fields into separate variables.
    var
      stmt = ast
      parentNode = pattern
    for i, n in y.pos:
      if i != y.pos.high:
        stmt = BracketExpr(stmt, Lit n)
        parentNode = parentNode[n]
    if $parentNode[y.pos[^1]][^1] == "*":
      let varargPos = parentNode.getVarargs()
      massert(varargPos.isSome,
        "Unable to do recurring pattern in node of kind: " & $parentNode.kind,
        parentNode[y.pos[^1]])
      massert(y.pos[^1] == varargPos.get().start,
        "Recurring pattern in wrong part of node",
        parentNode[y.pos[^1]])
      let
        start = Lit varargPos.get().start
        stop = Lit varargPos.get().stop
      if y.node.kind == nnkIdent:
        # Yes this AST is dumb, but a combined statement doesn't work..
        result.add superQuote do:
          when declared(`y.node`):
            when not isVariable(`y.node`):
              var `y.node`: seq[NimNode]
          else:
              var `y.node`: seq[NimNode]
      result.add superQuote do:
        for i in `start`.. `stmt`.len - `stop`:
          `y.node`.add `stmt`[i]
    else:
      stmt = BracketExpr(stmt, Lit y.pos[^1])
      if y.node.kind == nnkIdent:
        # Yes this AST is dumb, but a combined statement doesn't work..
        result.add superQuote do:
          when declared(`y.node`):
            when isVariable(`y.node`):
              `y.node` = `stmt`
            else:
              let `y.node` = `stmt`
          else:
            let `y.node` = `stmt`
      else:
        result.add Asgn(y.node, stmt)
