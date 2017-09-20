import Text.Show.Functions

type Identifier = String

type Env = [(Identifier, Value)]
data WAE = Num Int
         | Add WAE WAE
         | Id Identifier
         | With Identifier WAE WAE
         | Fun Identifier WAE
         | App WAE WAE
data Value = NumV Int
           | FunV (Value -> Value)
           deriving Show

interp :: WAE -> Env -> Value
interp (Num n) env = NumV n
interp (Add lhs rhs) env = add_numbers (interp lhs env) (interp rhs env)
interp (Id i) env = lookup_id i env
interp (With bound_id named_expr bound_body) env =
    interp bound_body
           ((bound_id, interp named_expr env) : env)
interp (Fun arg_id fun_body) env =
    FunV (\ arg_val -> interp fun_body
                              ((arg_id, arg_val) : env))
interp (App fun_expr arg_expr) env =
    let FunV the_fun = interp fun_expr env
    in the_fun (interp arg_expr env)

lookup_id :: Identifier -> Env -> Value
lookup_id var ((i, v) : r)
  | (var == i) = v
  | otherwise  = lookup_id var r

add_numbers :: Value -> Value -> Value
add_numbers (NumV a) (NumV b) = NumV (a + b)
