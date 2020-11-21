sealed trait Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr
case class Literal(value: Int) extends Expr
case class Variable(name: String) extends Expr

def stringify(expr: Expr): String = expr match {
  case Literal(value) => value.toString
  case Variable(name) => name
  case BinOp(left, op, right) => s"(${stringify(left)} $op ${stringify(right)})"
}

def simplify(expr: Expr): Expr = {
  expr match {
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
   
    case BinOp(Variable(name), "*", Literal(value)) => BinOp(Variable(name), "*", Literal(value))
    case BinOp(Literal(value), "*", Variable(name)) => BinOp(Literal(value), "*", Variable(name))

    case BinOp(left, "+", right) => simplify(BinOp(simplify(left), "+", simplify(right)))
    case BinOp(left, "-", right) => simplify(BinOp(simplify(left), "-", simplify(right)))
    case BinOp(left, "*", right) => simplify(BinOp(simplify(left), "*", simplify(right)))

    case Literal(value) => Literal(value)
    case Variable(name) => Variable(name)
  }
}
