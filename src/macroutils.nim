
import ast_pattern_matching
import options
import optionsutils

export options


func newBreakStmt*(): NimNode =
    nnkBreakStmt.newTree(newEmptyNode())

func newBreakStmt*(label: NimNode): NimNode =
    label.expectKind(nnkIdent)
    nnkBreakStmt.newTree(label)

func newBoolLitNode*(a: bool): NimNode =
    [ident"false", ident"true"][int a]

func newBracketExpr*(a: NimNode, b: varargs[NimNode]): NimNode =
    nnkBracketExpr.newTree(a).add(b)

func newIndex*(a: NimNode, b: int): NimNode =
    a.newBracketExpr(newIntLitNode(b))

func add*(father: NimNode, child: Option[NimNode]): NimNode =
    withSome child:
        some child:
            father.add child
        none:
            father

func add*(father: NimNode, children: Option[seq[NimNode]]): NimNode =
    withSome children:
        some children:
            father.add children
        none:
            father

func add*(father: NimNode, children: seq[Option[NimNode]]): NimNode =
    for child in children:
        withSome child:
            some child:
                father.add child
            none:
                discard
    father

func addElse*(ifStmt: NimNode, elseBranch: NimNode): NimNode =
    ## When `ifStmt` has no else branches, add to `elseBranch` `ifStmt`
    ifStmt.expectKind({nnkIfStmt, nnkIfExpr})
    elseBranch.expectKind({nnkElse, nnkElseExpr})
    if ifStmt[^1].kind in {nnkElse, nnkElseExpr}:
        ifStmt.add elseBranch
    ifStmt

func infix*(lhs, op, rhs: NimNode): NimNode =
    nnkInfix.newTree(op, lhs, rhs)
