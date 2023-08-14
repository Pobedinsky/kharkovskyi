type func = 
| Var of float
| E 
| PI
| X
| Log  of func * func
| Pow  of func * func
| Mult of func * func
| Sum  of func * func
| Sub  of func * func
| Div  of func * func;;


let rec string_of_func = function
| Var (f)    -> string_of_float(f)
| E          -> "e"
| PI         -> "π"
| X          -> "X"
| Pow (f, g) -> "( "  ^ string_of_func f ^ " ) ^ ( "^ string_of_func g ^ " )"
| Mult (f, g)-> "( "  ^ string_of_func f ^ " ) * ( "^ string_of_func g ^ " )"
| Div (f, g) -> "( "  ^ string_of_func f ^ " ) / ( "^ string_of_func g ^ " )"
| Sum (f, g) -> "( "  ^ string_of_func f ^ " ) + ( "^ string_of_func g ^ " )"
| Sub (f, g) -> "( "  ^ string_of_func f ^ " ) - ( "^ string_of_func g ^ " )"
| Log (f, g) -> "( Log ("  ^ string_of_func f ^ ") ( " ^ string_of_func g ^ " ))";;

let rec derive = function
| X               -> Var(1.)
| Pow (Var f, g) when f > 0. && f <> 1. -> Mult( Pow(Var f, g), Mult( Log(E, Var f), derive g))
| Pow (PI, g)     -> Mult( Pow(PI, g), Mult( Log(E, PI), derive g))
| Pow (E, g)      -> Mult( Pow(E, g), derive g)
| Pow (f, g) when g = E || g = PI -> Mult(g , Mult(Pow(f, Sub(g, Var(1.))), derive f))
| Pow (f, Var(g)) -> Mult(Var g , Mult(Pow(f, Var(g-.1.)), derive f))
| Pow (f, g)      -> Sum( Mult(g, Mult(Pow(f, Sub(g, Var(1.))), derive f)) , Mult( Pow(f,g), Mult(Log(E, f), derive(g))) )
| Mult (Var(f), g)-> Mult( Var f, derive g)
| Mult (f, g)     -> Sum( Mult( derive f, g), Mult(f, derive g ) )
| Div (f, g)      -> Div( Sub( Mult(derive f, g), Mult(f, derive g ) ), Pow(g, Var(2.)) )
| Sum (f, g)      -> Sum( derive f, derive g )
| Var _  | E | PI -> Var(0.)
| Log (Var f, g)  -> Mult( Div( derive g, g ), Log(Var f, E) )
| Log (PI, g)     -> Mult( Div( derive g, g ), Log(PI, E) )
| Log (E, g)      -> Div( derive g, g )
| _ -> failwith "Not done yet";;


let testing = Sum( Pow(X, Pow(X,Var(2.))), E)

let () = testing |> derive |> string_of_func |> print_string
