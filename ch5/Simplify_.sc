sealed trait Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr
case class Literal(value: Int) extends Expr
case class Variable(name: String) extends Expr

def stringify(expr: Expr): String = expr match {
  case BinOp(left, op, right) => s"(${stringify(left)} $op ${stringify(right)})"
  case Literal(value) => value.toString
  case Variable(name) => name
}

def simplify(expr: Expr): Expr = {
  val res: Expr = expr match {
    case BinOp(Literal(left), "+", Literal(right)) => Literal(left + right)
    case BinOp(Literal(left), "-", Literal(right)) => Literal(left - right)
    case BinOp(Literal(left), "*", Literal(right)) => Literal(left * right)

    case BinOp(left, "+", Literal(0)) => simplify(left)
    case BinOp(Literal(0), "+", right) => simplify(right)
    
    case BinOp(left, "-", Literal(0)) => simplify(left)

    case BinOp(left, "*", Literal(1)) => simplify(left)
    case BinOp(Literal(1), "*", right) => simplify(right)

    case BinOp(left, "*", Literal(0)) => Literal(0)
    case BinOp(Literal(0), "*", right) => Literal(0)

    case BinOp(left, "+", right) => BinOp(simplify(left), "+", simplify(right))
    case BinOp(left, "-", right) => BinOp(simplify(left), "-", simplify(right))
    case BinOp(left, "*", right) => BinOp(simplify(left), "*", simplify(right))

    case Literal(value) => Literal(value)
    case Variable(name) => Variable(name)
  }
  // recursive call
  if (res == expr) res
  else simplify(res)
}
