import Text.Show.Functions

type Identifier = String

type Env = [(Identifier, Value)]
data RCFWAE = Num Int
            | Add RCFWAE RCFWAE
            | Id Identifier
            | With Identifier RCFWAE RCFWAE
            | Fun Identifier RCFWAE
            | App RCFWAE RCFWAE
            | If0 RCFWAE RCFWAE RCFWAE
            | Rec Identifier RCFWAE RCFWAE
data Value = NumV Int
           | FunV (Value -> Value)
           deriving Show

interp :: RCFWAE -> Env -> Value
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
interp (If0 condition t_branch f_branch) env =
    let NumV c = interp condition env
    in if c == 0
        then interp t_branch env
        else interp f_branch env
interp (Rec bound_id named_expr bound_body) env =
    interp bound_body
           (cyclically_bind_and_interp bound_id named_expr env)

lookup_id :: Identifier -> Env -> Value
lookup_id var ((i, v) : r)
  | (var == i) = v
  | otherwise = lookup_id var r

add_numbers :: Value -> Value -> Value
add_numbers (NumV a) (NumV b) = NumV (a + b)

cyclically_bind_and_interp :: Identifier -> RCFWAE -> Env -> Env
cyclically_bind_and_interp id (Fun param body) env =
    let rec_ext_env = (id, FunV (\ arg_val -> interp body ((param, arg_val) : rec_ext_env))) : env
    in rec_ext_env
