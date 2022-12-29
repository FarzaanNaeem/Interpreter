(* Honor code comes here:

   First Name: Farzaan
   Last Name: Naeem
   BU ID: U03902484

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)

(*Writing a line to a file*)

type stack_val = 
|Int of int
|String of string
|Clo of string * string * string list * bool
|Tuple of stack_val list
|Union of union

and union = 
|Left of stack_val
|Right of stack_val

(*-----------------------------Helper Functions-----------------------------*)
(*A helper function that gives us the length of the list so that we can see if the list's length is > 2
   which will be important for majority of these functions*)
let rec sls_count (sls:stack_val list) = 
  match sls with 
  |[] -> 0
  |hd::tl -> 1 + sls_count tl

(*If the bool value of IfThen is true, then when we get to else, we need to skip those commands and this function
   helps us do that*)
let rec skip_else_commands (parse_list: string list) = 
  match parse_list with
  |[] -> []
  |hd::tl ->
    if(hd = "End") then
      parse_list
    else
      skip_else_commands tl 
(*If the bool value of IfThen is false, then we need to skip the first set of commands until we get to "Else"
   which will be the commands that we implement into our stack val*)
let rec skip_ifthen_commands (parse_list: string list) = 
  match parse_list with
  |[] -> []
  |hd::tl ->
    if(hd = "Else") then
      tl
    else 
      skip_ifthen_commands tl

(*This helper function checks to see if there is a variable with that specific name*)
let rec is_var (var_name: string) (var_lst: (string*stack_val) list) = 
  match var_lst with
  |[] -> false
  |hd::tl ->
    match hd with
    |(val1, _) -> 
      if(val1 = var_name) then 
        true
      else
        is_var var_name tl

let store_func_vars (var_name: string) (value:stack_val) (var_lst: (string*stack_val) list)= 
      (var_name, value)::var_lst

(*This function is only enacted if there is either a local or global variable. This looks for the variable 
   and returns the value associated with it*)
let rec get_var (var_name: string) (var_lst: (string*stack_val) list) = 
  match var_lst with
  |[] -> String("Error")
  |hd::tl ->
    match hd with
    |(var, value) -> 
      if (var = var_name) then
        value
      else
        get_var var_name tl

let rec is_func (var_name: string) (func_lst: stack_val list) = 
  match func_lst with
  |[] -> false
  |hd::tl ->
    match hd with
    |Int _ |String _ |Tuple _ |Union _ -> false
    |Clo(func_name, _, _, _) ->
      if(var_name = func_name) then
        true
      else
        is_func (var_name) tl

let rec get_func (func: string) (func_lst: stack_val list) = 
  match func_lst with
  |[] -> String "Error"
  |hd::tl ->
    match hd with
    |Int _ |String _ |Tuple _ |Union _ -> String("Error")
    |Clo(func_name, arg, cmds, _) ->
      if(func_name = func) then
        hd
      else
        get_func func tl

let rec skip_caseleft(parse_list: string list) (counter: int) = 
match parse_list with
|[] -> []
|hd::tl ->
  if(hd = "End") then
    skip_caseleft tl (counter-1)
  else if(hd = "Right" && counter=0) then
    tl
  else if(hd = "CaseLeft") then
    skip_caseleft (tl) (counter+1)
  else
    skip_caseleft (tl) (counter) 

let rec skip_right(parse_list: string list) (counter:int) = 
match parse_list with
|[] -> []
|hd::tl ->
  if(hd = "CaseLeft") then 
    skip_right (tl) (counter+1)
  else if(hd = "End" && counter=0) then
    tl
  else if (hd = "End" && counter > 0) then
    skip_right (tl) (counter-1)
  else 
    skip_right tl counter

let rec preserve_funcs (mut_cmds: stack_val list) (func_list: stack_val list) = 
  match mut_cmds with
  |[] -> func_list
  |hd::tl -> hd::preserve_funcs tl func_list
let rec get_mut_cmds(parse_list: string list) = 
  match parse_list with
  |[] -> "Error"::parse_list
  |hd::tl -> 
    if(hd = "End" (*|| hd = "Return"*)) then
      hd::[]
    else
      hd:: get_mut_cmds tl

let rec skip_mut_cmds (parse_list: string list) = 
  match parse_list with
  |[] -> "Error"::parse_list
  |hd::tl ->
   if(hd = "Return") then
      tl
    else
      skip_mut_cmds tl  

let rec get_func_cmds (parse_list: string list) (count: int) = 
  let rec get_func_list (parse_lst:string list) (count2: int) = 
    match parse_lst with
    |[] -> "Error"::parse_list
    |hd::tl -> 
      if(hd = "Mut") then 
        get_func_list (skip_mut_cmds tl) (count2)
     else 
      if(hd = "IfThen" || hd = "Begin" || String.sub hd (0) (3) = "Fun") then
       hd::get_func_list tl (count2+1)
      else if (hd = "End") then
        if(count2 < 1) then
          hd::[]
        else
          hd::get_func_list (tl) (count2-1)
      else
        hd::get_func_list tl count2
  in let rec get_rest_parse (length:int) (parse_lst2:string list) = 
    match length with
    |0 -> parse_lst2
    |_ ->
      match parse_lst2 with
      |[]-> "Error"::parse_lst2
      |hd::tl -> 
        get_rest_parse (length-1) (tl)
    in let rec get_mut_list (parse_list: string list) = 
      match parse_list with
      |[] -> []
      |hd::tl ->
        if(String.sub hd 0 3 = "Mut") then
          let mut_func = String.split_on_char (' ') (String.sub hd 4 ((String.length hd)-4)) in
          (match mut_func with
          |func_name::func_var::[] -> Clo(func_name, func_var, get_mut_cmds tl, true)::get_mut_list tl)
        else
          get_mut_list tl
  in (get_func_list (parse_list) count, get_rest_parse (List.length (get_func_list(parse_list) count)) parse_list, get_mut_list (parse_list))

  let rec get_func_index(func_name:string)(curr_i:int)(func_lst: stack_val list) = 
    match func_lst with
    |[] -> -1
    |hd::tl ->
      match hd with
      |Int _ |String _ |Tuple _ |Union _ -> -1
      |Clo(func_name2, _, _, _) -> 
        if(func_name2 = func_name) then
          curr_i
      else
        get_func_index func_name (curr_i+1) tl 
(*----------------------Main Functions that are used for interpreter----------------------*)

let rec print_tuple (begin_string: string) (value: stack_val list) = 
  match value with
  |[] -> "()"
  |Int(hd)::[] |Union(Left(Int(hd)))::[] -> (string_of_int hd) ^ ")"
  |Int(hd)::tl -> begin_string ^ (string_of_int hd) ^ ", " ^ print_tuple "" tl
  |String(hd)::[] | Union(Left(String(hd)))::[] -> hd ^ ")"
  |String(hd)::tl -> begin_string ^ hd ^ ", " ^ print_tuple "" tl
  |Clo(hd, var, _, _)::tl -> begin_string ^ "Clo (" ^ hd ^ ", " ^ var ^ "), " ^ print_tuple "" tl 
  |Tuple(value)::tl -> begin_string ^ (print_tuple "(" value) ^ ", " ^ print_tuple "" tl  
  |Union(value)::tl -> 
    (match value with
    |Left(value) -> begin_string ^ "Left " ^ (convert_val_to_string value) ^ ", " ^ print_tuple "" tl
    |Right(value) -> begin_string ^ "Right " ^ (convert_val_to_string value) ^ ", " ^ print_tuple "" tl
    )

(*This converts all values into strings so that we can produce an output in the output file*)
and convert_val_to_string(value: stack_val) = 
  match value with
  |Int(value) -> string_of_int value
  |Union(Left(value)) -> "Left " ^ convert_val_to_string value
  |Union(Right(value)) -> "Right " ^ convert_val_to_string value
  |Clo(value, var, _, _) -> "Clo (" ^ value ^ ", " ^ var ^ ")"
  |Tuple(value) -> print_tuple "(" value
  |String(value) -> value

let rec get_sublist (length: int) (sls: stack_val list) = 
  match length with
  |0 -> []
  |_ ->
    match sls with 
    |[] -> String("Error")::sls
    |hd :: tl -> hd::get_sublist (length-1) tl
  (*
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp*)

let write_file_parse (sls: stack_val list) (file_path: string) =
  let fp = open_out file_path in 
  let () = List.fold_left(fun acc x -> 
    let () = Printf.fprintf fp "%s\n" (convert_val_to_string x) in ()) () sls in
    close_out fp

(*If at any point there is an error, we call on this function to write "Error" into the output file*)
let error (file_path: string) = 
  let fp = open_out file_path in
  let () = Printf.fprintf fp "\"Error\"" in
    close_out fp

let empty (file_path: string) = 
  let fp = open_out file_path in
    let () = Printf.fprintf fp "" in
      close_out fp
(*This will parse the commands into a string list so we can easily read the commands*)
let parse (src: string) =
  Str.split(Str.regexp "\n") src

(*This is invoked if the push command is called. If it is, then we add the new value onto the stack*)
let push (value: stack_val) (sls: stack_val list): stack_val list = 
  value :: sls

(*Called upon when the command pop is called. If it is, the we remove the top value from the stack*)
let pop (sls: stack_val list) = 
  match sls with
  |hd::[] -> []
  |[] -> push (String("Error")) sls
  |hd::tl -> tl

(*Called upon when the command add is called. If it is, we add 2 values from the top of the stack together*)
let add (sls: stack_val list) = 
  if sls_count sls < 2 then
    String("Error")
  else 
    match sls with
    |[]-> String("Error")
    |val1::[] -> String("Error")
    |val1::val2::tl ->
      match val1, val2 with
      |Int(val1), Int(val2) -> Int(val1 + val2)
      |_, String(val2) -> String("Error")
      |Union(_), _ -> String("Error")
      |_, Union(_) -> String("Error")
      |Tuple(_), _ -> String("Error")
      |_, Tuple(_) -> String("Error")
      |Clo(_), _ -> String("Error")
      |_, Clo(_) -> String("Error")
      |String(val1), _ -> String("Error")

(*Called upon when the command sub is called. We subtract 2 values from the top of the stack*)
let sub (sls: stack_val list) = 
  if sls_count sls < 2 then
    String("Error")
  else 
    match sls with
    |[]-> String("Error")
    |val1::[] -> String("Error")
    |val1::val2::tl ->
      match val1, val2 with
      |Int(val1), Int(val2) -> Int(val1 - val2)
      |_, String(val2) -> String("Error")
      |Union(_), _ -> String("Error")
      |_, Union(_) -> String("Error")
      |Tuple(_), _ -> String("Error")
      |_, Tuple(_) -> String("Error")
      |Clo(_), _ -> String("Error")
      |_, Clo(_) -> String("Error") 
      |String(val1), _ -> String("Error")
  
(*Called when mul command is called. Multiply 2 values from top of the stack*)
let mul (sls: stack_val list) = 
  if sls_count sls < 2 then
    String("Error")
  else 
    match sls with
    |[]-> String("Error")
    |val1::[] -> String("Error")
    |val1::val2::tl ->
      match val1, val2 with
      |Int(val1), Int(val2) -> Int(val1 * val2)
      |_, String(val2) -> String("Error")
      |Union(_), _ -> String("Error")
      |_, Union(_) -> String("Error")
      |Tuple(_), _ -> String("Error")
      |_, Tuple(_) -> String("Error")
      |Clo(_), _ -> String("Error")
      |_, Clo _ -> String("Error")
      |String(val1), _ -> String("Error")

(*Called when div is the command. Divide 2 values from each other*)
let div (sls: stack_val list) =
  if sls_count sls < 2 then
    String("Error")
  else 
    match sls with
    |[]-> String("Error")
    |val1::[] -> String("Error")
    |val1::val2::tl ->
      match val1, val2 with
      |Int(val1), Int(val2) -> 
        if val2 = 0 then
          String("Error")
        else
          Int(val1 / val2)
      |_, String(val2) -> String("Error")
      |Union(_), _ -> String("Error")
      |_, Union(_) -> String("Error")
      |Tuple(_), _ -> String("Error")
      |_, Tuple(_) -> String("Error")
      |Clo(_), _ -> String("Error")
      |_, Clo _ -> String("Error")
      |String(val1), _ -> String("Error")

(*Swaps the top 2 values in the stack.*)
let swap (sls: stack_val list) = 
  if(sls_count sls < 2) then
    String("Error") :: sls
  else
    match sls with 
    |[] -> push (String("Error")) sls
    |val1::[] -> push (String("Error")) sls
    |val1::val2::tl -> val2::val1::tl

(*We multiply the top value of the stack by -1*)
let neg (sls: stack_val list) = 
  match sls with 
  |[] -> String("Error")
  |hd::tl -> 
    match hd with
    |String(hd) -> String("Error")
    |Union(_) -> String("Error")
    |Tuple(_) -> String("Error")
    |Clo(_) -> String("Error")
    |Int(hd) -> Int(hd * -1)

let concat (sls: stack_val list) = 
  if sls_count sls < 2 then
    String("Error")
  else 
    match sls with
    |[] -> String("Error")
    |Union(_)::_:: tl |_::Union(_)::tl -> String "Error"
    |Union(_)::[] -> String "Error"
    |Tuple(_)::_::tl |_::Tuple(_)::tl -> String "Error"
    |Tuple(_)::[] -> String("Error")
    |Clo(_)::_::tl |_::Clo(_)::tl -> String("Error")
    |Clo(_)::[] -> String("Error")
    |Int(val1)::tl |_::Int(val1)::tl -> String("Error")
    |String(str1)::[] -> String("Error")
    |String(str1)::String(str2)::tl -> 
      String((String.sub (str1) (0) ((String.length str1)-1)) ^ (String.sub (str2) (1) ((String.length str2)-1)))

let and_func (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |_::Union(_)::tl -> String("Error")
  |Tuple(_)::_::tl -> String("Error")
  |_::Tuple(_)::tl -> String("Error")
  |Tuple(_)::[] -> String("Error")
  |Clo(_)::_::tl -> String("Error")
  |_::Clo(_)::tl -> String("Error")
  |Clo(_)::[] -> String("Error")
  |String(val1)::tl -> String("Error")
  |_::String(val1)::tl -> String("Error")
  |Int(val1)::[] -> String("Error")
  |Int(val1)::Int(val2)::tl -> 
    if(val1 > 1 || val2 > 1 || val1 < 0 || val2 < 0) then
      String("Error")
    else if(val1 = val2) then
      Int(1)
    else 
      Int(0)

let or_func (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |_::Union(_)::tl -> String("Error")
  |Tuple(_)::_::tl -> String("Error")
  |_::Tuple(_)::tl -> String("Error")
  |Tuple(_)::[] -> String("Error")
  |Clo(_)::_::tl -> String("Error")
  |_::Clo(_)::tl -> String("Error")
  |Clo(_)::[] -> String("Error")
  |String(val1)::tl -> String("Error")
  |_::String(val1)::tl -> String("Error")
  |Int(val1)::[] -> String("Error")
  |Int(val1)::Int(val2)::tl -> 
    if(val1 > 1 || val2 > 1 || val1 < 0 || val2 < 0) then
      String("Error")
    else if(val1 = 1 || val2 = 1) then
      Int(1)
    else 
      Int(0)

(*This checks to see if one value is less than or equal another*)
let lte (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |_::Union(_)::tl -> String("Error")
  |Tuple(_)::_::tl -> String("Error")
  |_::Tuple(_)::tl -> String("Error")
  |Tuple(_)::[] -> String("Error")
  |Clo(_)::_::tl -> String("Error")
  |_::Clo(_)::tl -> String("Error")
  |Clo(_)::[] -> String("Error")
  |String(val1)::tl -> String("Error")
  |_::String(val1)::tl -> String("Error")
  |Int(val1)::[] -> String("Error")
  |Int(val1)::Int(val2)::tl -> 
    if(val1 <= val2) then
      Int(1)
    else 
      Int(0)

(*This is the not operator for booleans. If a value is 0, it becomes one and vice versa*)
let not (sls:stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |_::Union(_)::tl -> String("Error")
  |Tuple(_)::tl -> String("Error")
  |Clo(_)::tl -> String("Error")
  |String(val1)::tl -> String("Error")
  |Int(val1)::tl -> 
    if(val1 > 1 || val1 < 0) then
      String("Error")
    else if(val1 = 1) then
      Int(0)
    else
      Int(1)

(*This functions checks to see if two ints are equal to each other*)
let equal (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |_::Union(_)::tl -> String("Error")
  |Tuple(_)::_::tl -> String("Error")
  |_::Tuple(_)::tl -> String("Error")
  |Tuple(_)::[] -> String("Error")
  |Clo(_)::_::tl -> String("Error")
  |_::Clo(_)::tl -> String("Error")
  |Clo(_)::[] -> String("Error")
  |String(val1)::tl -> String("Error")
  |_::String(val1)::tl -> String("Error")
  |Int(val1)::[] -> String("Error")
  |Int(val1)::Int(val2)::tl -> 
    if(val1 = val2) then
      Int(1)
    else 
      Int(0)
  
(*This function checks to see if we should do the commands after ifthen or the commands after else*)
let ifthen (sls:stack_val list) = 
  match sls with
  |[] -> String("Error")
  |Union(_)::tl -> String("Error")
  |Tuple(_)::tl -> String("Error")
  |Clo(_)::tl -> String("Error")
  |String(val1)::tl -> String("Error")
  |Int(val1)::tl ->
    if(val1 > 1 || val1 < 0) then
      String("Error")
    else if(val1 = 1) then
      Int(1)
    else 
      Int(0)

(*This function helps to store the values in their correct list depending on if they are global or local with their name and value*)
let store_vars (var_name: string) (sls: stack_val list) (var_list: (string*stack_val) list) = 
  match sls with
  |[] -> []
  |hd::tl -> (var_name, hd)::var_list

(*This will make the current value on the stack a left union value*)
let left_inj (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |hd::tl -> Union(Left(hd))

(*This gives us the right union value*)
let right_inj (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |hd::tl -> Union(Right(hd))

(*This function will analyze caseleft and check to see if we should perform "caseleft" or "right" commands*)
let caseleft (sls: stack_val list) = 
  match sls with
  |[] -> (Int(0), String("Error"))
  |String(value)::tl -> (Int(0), String("Error"))
  |Int(value)::tl -> (Int(0), String("Error"))
  |Tuple(_)::tl -> (Int(0), String("Error"))
  |Clo(_)::tl -> (Int(0), String("Error"))
  |Union(Left(value))::tl -> (Int(1), value)
  |Union(Right(value))::tl -> (Int 0, value)

let right (sls: stack_val list) = 
  match sls with
  |[] -> String("Error")
  |String(value)::tl -> String("Error")
  |Int(value)::tl -> String("Error")
  |Tuple(_)::tl -> String("Error")
  |Clo(_)::tl -> String("Error")
  |Union(Left(value))::tl -> Int 0
  |Union(Right(value))::tl -> value

(*This function will help to create the tuple*)
let rec create_tuple (length: int) (sls: stack_val list) =
  (*A helper function that will actually create the tuple list*)
  let rec tuple (length: int) (sls: stack_val list) = 
    match length with 
    |0 -> []
    |_ -> 
      match sls with
      |[] -> String("Error")::sls
      |hd::tl -> hd::tuple(length-1) tl in
    let rec pop_sls (length: int) (sls: stack_val list) =  
      match length with
      |0 -> sls
      |_ -> 
        match sls with
        |[] -> String("Error") :: sls
        |hd::tl -> pop_sls (length-1) tl
    in (tuple(length) (List.rev (get_sublist length (sls))), pop_sls (length) (sls))

(*This performs the get operation where we get the value desired from the tuple*)
let rec get_tuple_val (curr_i: int) (val_index: int) (tls: stack_val) = 
  match tls with 
  |Int _-> String("Error")
  |String _ -> String("Error")
  |Union _ -> String("Error")
  |Tuple([]) -> String("Error")
  |Clo _ -> String("Error")
  |Tuple(hd::tl) -> 
    if(curr_i = val_index) then
      hd
    else 
      get_tuple_val (curr_i+1) (val_index) (Tuple(tl)) 

let rec perform_func_cmds (parse_list: string list) (curr_func:string) (curr_mut:bool) (sls: stack_val list) (local_vars: (string*stack_val) list) 
(global_vars: (string*stack_val) list) (func_list: stack_val list) = 
  match parse_list with
  |[] -> (String "Error", global_vars)
  |hd::tl ->
    match hd with
    |"Pop" ->  
      if(sls_count sls < 1) then
        (String "Error", global_vars)
    else 
      let pop_result = pop sls in 
      perform_func_cmds tl curr_func curr_mut pop_result local_vars global_vars func_list
    |"Add" -> let add_result = add sls in 
      if(add_result = String("Error")) then
        (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (add_result) (pop(pop sls))) local_vars global_vars func_list
    |"Sub" -> let sub_result = sub sls in 
      if(sub_result = String("Error")) then
        (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (sub_result) (pop(pop sls))) local_vars global_vars func_list
    |"Mul" -> let mul_result = mul sls in 
      if(mul_result = String("Error")) then
        (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (mul_result) (pop(pop sls))) local_vars global_vars func_list
    |"Div" -> let div_result = div sls in 
      if(div_result = String("Error")) then
        (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (div_result) (pop(pop sls))) local_vars global_vars func_list
    |"Swap" -> let swap_result = swap sls in
      if(List.hd swap_result = String("Error")) then
        (String "Error", global_vars)
      else 
        perform_func_cmds tl curr_func curr_mut swap_result local_vars global_vars func_list
    |"Neg" -> let neg_result = neg sls in
      if(neg_result = String("Error")) then
        (String "Error", global_vars)
      else 
        perform_func_cmds tl curr_func curr_mut (push (neg_result) (pop sls)) local_vars global_vars func_list
    |"Concat" -> let concat_result = concat sls in 
      if(concat_result = String("Error")) then
        (String "Error", global_vars)
      else 
        perform_func_cmds tl curr_func curr_mut (push (concat_result) (pop(pop sls))) local_vars global_vars func_list
    |"And" -> let and_result = and_func sls in 
        if(and_result = String("Error")) then
          (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (and_result) (pop(pop sls))) local_vars global_vars func_list
    |"Or" -> let or_result = or_func sls in
        if(or_result = String("Error")) then
          (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (or_result) (pop(pop sls))) local_vars global_vars func_list
    |"Lte" -> let lte_result = lte sls in 
        if(lte_result = String("Error")) then
          (String "Error", global_vars)
        else
          perform_func_cmds tl curr_func curr_mut (push (lte_result) (pop(pop sls))) local_vars global_vars func_list
    |"Equal" -> let equal_result = equal sls in
        if(equal_result = String("Error")) then
          (String "Error", global_vars)
        else
          perform_func_cmds tl curr_func curr_mut (push (equal_result) (pop(pop sls))) local_vars global_vars func_list
    |"Not" -> let not_result = not sls in 
        if(not_result = String("Error")) then
          (String "Error", global_vars)
        else
          perform_func_cmds tl curr_func curr_mut (push (not_result) (pop(sls))) local_vars global_vars func_list
    |"IfThen" -> let ifthen_result = ifthen sls in
          if(ifthen_result = String("Error")) then
            (String "Error", global_vars)
          else if(ifthen_result = Int(1)) then
            perform_func_cmds tl curr_func curr_mut (pop sls) local_vars global_vars func_list
          else
            perform_func_cmds (skip_ifthen_commands tl) curr_func curr_mut (pop sls) local_vars global_vars func_list
    |"Else" -> perform_func_cmds (skip_else_commands tl) curr_func curr_mut (sls) local_vars global_vars func_list
    |"End" ->
      (String "Error", global_vars)
    |"Begin" -> let begin_result = begin_end tl [] local_vars global_vars func_list [] in
      (match begin_result with
      |(parse_tl, hd, global_var_lst)->
        if(hd = String("Error")) then
          (String "Error", global_var_lst)
        else
          perform_func_cmds parse_tl curr_func curr_mut (push hd sls) local_vars global_var_lst func_list)
    |"InjL" -> let injl_res = left_inj sls in
      if(injl_res = String("Error")) then
        (String "Error", global_vars)
      else 
        perform_func_cmds tl curr_func curr_mut (push (injl_res) (pop(sls))) local_vars global_vars func_list
    |"InjR" -> let injr_res = right_inj sls in
      if(injr_res = String("Error")) then
        (String "Error", global_vars)
      else
        perform_func_cmds tl curr_func curr_mut (push (injr_res) (pop(sls))) local_vars global_vars func_list 
    |"CaseLeft" -> let caseleft_res = caseleft sls in
        (match caseleft_res with
        |(bool_check, value) -> 
          if(value = String "Error") then
            (String "Error", global_vars)
        else if(bool_check = Int 0) then
          perform_func_cmds (skip_caseleft tl 0) curr_func curr_mut (push (right(sls)) (pop(sls))) local_vars global_vars func_list
        else 
          perform_func_cmds tl curr_func curr_mut (push value (pop (sls))) local_vars global_vars func_list)
    |"Right" -> perform_func_cmds (skip_right tl 0) curr_func curr_mut sls local_vars global_vars func_list
    |"Return" -> 
      (match sls with
      |[] -> (String "Error", global_vars)
      |hd::tl -> (hd, global_vars))
    |"Call" ->
      (match sls with
      |[] -> (String "Error", global_vars)
      |hd::[] -> (String "Error", global_vars)
      |hd::hd2::tl2 ->
        match hd with
        |Int _ |String _ |Tuple _|Union _ -> (String "Error", global_vars)
        |Clo(func_name, var_name, func_cmds, is_mut) -> 
          if(get_func_index (func_name) 0 (func_list) >= get_func_index curr_func 0 func_list || is_mut || curr_mut) then
          let func_result = perform_func_cmds func_cmds func_name is_mut [] (store_func_vars var_name hd2 local_vars) global_vars func_list in
        (match func_result with
        |(func_value, new_global_vars) ->
        perform_func_cmds tl curr_func curr_mut (push (func_value) (tl2)) local_vars new_global_vars func_list)
        else 
          (String "Error", global_vars))
    |"Quit" -> (String "Error", global_vars)
    |_ ->
      (*This is where we check to see if the command is either Push, Local, Global, or Tuple*)
      if(String.sub hd (0) (3) = "Fun") then
        let func_name = String.split_on_char(' ') (String.sub hd 4 ((String.length hd)-4)) in
        let func_cmds = get_func_cmds parse_list 0 in
        (match func_cmds with
        |(fun_cmds, parse_tl, mut_cmds) -> 
          if(List.hd fun_cmds = "Error") then
            (String "Error", global_vars)
        else
          (match func_name with
          |[] -> (String "Error", global_vars)
          |_::[] -> (String "Error", global_vars)
          |fun_name::fun_var::tl -> perform_func_cmds parse_tl curr_func curr_mut (sls) local_vars global_vars (Clo(fun_name, fun_var, fun_cmds, false)::preserve_funcs (mut_cmds) func_list)))
      else if(String.sub (hd) (0) (5) = "Tuple") then
        if((int_of_string (String.sub (hd) (6) ((String.length hd)-6))) <= List.length sls) then
          let tuple_result = create_tuple (int_of_string (String.sub (hd) (6) ((String.length hd)-6))) (sls) in
          (match tuple_result with 
          |(tuple_ls, sls_tl) -> 
          (match sls_tl with
            |[] -> perform_func_cmds tl curr_func curr_mut (push (Tuple(tuple_ls)) sls_tl) local_vars global_vars func_list
            |String "Error" :: _ -> (String "Error", global_vars)
            |hd::tl2 -> perform_func_cmds tl curr_func curr_mut (push (Tuple(tuple_ls)) sls_tl) local_vars global_vars func_list))
        else
          (String "Error", global_vars)
      else if(String.sub (hd) (0) (3) = "Get") then
        if(List.length sls < 1 || (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) >= List.length sls) then
          (String "Error", global_vars)
        else
          (match List.hd sls with
          |String _ -> (String "Error", global_vars)
          |Int _ -> (String "Error", global_vars)
          |Union _ -> (String "Error", global_vars)
          |Clo _ -> (String "Error", global_vars)
          |Tuple(value) -> let get_result = get_tuple_val 0 (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) (Tuple(value)) in 
          if(get_result = String("Error")) then
            (String "Error", global_vars)
          else
            perform_func_cmds tl curr_func curr_mut (push get_result sls) local_vars global_vars func_list )
      else if(String.sub (hd) (0) (4) = "Push") then
        if (is_func (String.sub hd (5) ((String.length hd)-5)) func_list) then
          let push_result = push (get_func (String.sub hd (5) ((String.length hd)-5)) (func_list)) sls in
          perform_func_cmds tl curr_func curr_mut push_result local_vars global_vars func_list
      else if(is_var(String.sub hd 5 ((String.length hd)-5)) local_vars) then
          let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) local_vars) sls in
          perform_func_cmds tl curr_func curr_mut push_result local_vars global_vars func_list 
      else if(is_var(String.sub hd 5 ((String.length hd)-5)) global_vars) then
        let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) global_vars) sls in
        perform_func_cmds tl curr_func curr_mut push_result local_vars global_vars func_list 
      else if(Str.string_match(Str.regexp "[0-9|-0--9]+$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
         let push_result = push (Int(int_of_string(String.sub (hd) (5) ((String.length hd)-5)))) sls in
         perform_func_cmds tl curr_func curr_mut push_result local_vars global_vars func_list 
        else if(Str.string_match(Str.regexp "\"[a-z|A-Z]+\"$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
          let push_result = push (String((String.sub (hd) (5) ((String.length hd)-5)))) sls in
          perform_func_cmds tl curr_func curr_mut push_result local_vars global_vars func_list 
        else 
          (String "Error", global_vars)
        else if(String.sub hd 0 5 = "Local") then 
          if(List.length sls < 1) then
            (String "Error", global_vars)
          else if(String.length hd = 7 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            perform_func_cmds tl curr_func curr_mut (pop(sls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (sls) (local_vars)) global_vars func_list 
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            perform_func_cmds tl curr_func curr_mut (pop(sls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (sls) (local_vars)) global_vars func_list 
          else
            (String "Error", global_vars)
        else if(String.sub hd 0 6 = "Global") then
          if(List.length sls < 1) then 
            (String "Error", global_vars)
          else if(String.length hd = 8 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            perform_func_cmds tl curr_func curr_mut (pop(sls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (sls) (global_vars)) func_list 
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            perform_func_cmds tl curr_func curr_mut (pop(sls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (sls) (global_vars)) func_list
          else
            (String "Error", global_vars)
        else
          (String "Error", global_vars)

(*May need to change this to return type Unit*)
and begin_end (parse_list: string list) (tls: stack_val list) (local_vars:(string * stack_val) list) 
(global_vars:(string*stack_val) list) (func_lst: stack_val list) (nested_stmts: stack_val list)= 
  match parse_list with
  |[] ->([], String("Error"), global_vars)
  |hd::tl ->
    match hd with
    |"Pop" ->  
      if(sls_count tls < 1) then
        (tl, String("Error"), global_vars)
    else 
      let pop_result = pop tls in 
        begin_end tl pop_result local_vars global_vars func_lst nested_stmts
    |"Add" -> let add_result = add tls in 
      if(add_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else
        begin_end tl (push (add_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Sub" -> let sub_result = sub tls in 
      if(sub_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else
        begin_end tl (push (sub_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Mul" -> let mul_result = mul tls in 
      if(mul_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else
        begin_end tl (push (mul_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Div" -> let div_result = div tls in 
      if(div_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else
        begin_end tl (push (div_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Swap" -> let swap_result = swap tls in
      if(List.hd swap_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else 
        begin_end tl swap_result local_vars global_vars func_lst nested_stmts
    |"Neg" -> let neg_result = neg tls in
      if(neg_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else 
        begin_end tl (push (neg_result) (pop tls)) local_vars global_vars func_lst nested_stmts
    |"Concat" -> let concat_result = concat tls in 
      if(concat_result = String("Error")) then
        (tl, String("Error"), global_vars)
      else 
        begin_end tl (push (concat_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"And" -> let and_result = and_func tls in 
        if(and_result = String("Error")) then
          (tl, String("Error"), global_vars)
      else
        begin_end tl (push (and_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Or" -> let or_result = or_func tls in
        if(or_result = String("Error")) then
          (tl, String("Error"), global_vars)
      else
        begin_end tl (push (or_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Lte" -> let lte_result = lte tls in 
        if(lte_result = String("Error")) then
          (tl, String("Error"), global_vars)
        else
          begin_end tl (push (lte_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Equal" -> let equal_result = equal tls in
        if(equal_result = String("Error")) then
          (tl, String("Error"), global_vars)
        else
          begin_end tl (push (equal_result) (pop(pop tls))) local_vars global_vars func_lst nested_stmts
    |"Not" -> let not_result = not tls in 
        if(not_result = String("Error")) then
          (tl, String("Error"), global_vars)
        else
          begin_end tl (push (not_result) (pop(tls))) local_vars global_vars func_lst nested_stmts
    |"IfThen" -> 
      if(ifthen(tls)=String("Error")) then
        (tl, String("Error"), global_vars)
      else if(ifthen tls = Int(1)) then
        begin_end tl tls local_vars global_vars func_lst (push (String("IfThen")) nested_stmts)
      else
        begin_end (skip_ifthen_commands tl) tls local_vars global_vars func_lst (push (String("IfThen")) nested_stmts)
    |"Else" -> begin_end (skip_else_commands tl) tls local_vars global_vars func_lst (nested_stmts)
    |"Begin" -> let begin_result = begin_end tl [] local_vars global_vars func_lst (push (String("Begin")) nested_stmts) in
        (match begin_result with
        |(parse_tl, hd, new_global_vars)->
          if(hd = String("Error")) then
            (tl, String("Error"), global_vars) 
          else
            begin_end (parse_tl) (push hd tls) local_vars new_global_vars func_lst (pop(nested_stmts)))
    |"End" -> 
      if(List.length nested_stmts < 1) then
        if(List.length tls < 1) then
          (parse_list, String("Error"), global_vars)
        else
          (parse_list, List.hd tls, global_vars)
      else if(List.hd nested_stmts = String("Begin")) then
        if(List.length tls < 1) then
          (parse_list, String("Error"), global_vars)
        else
          (parse_list, List.hd tls, global_vars)
      else
        begin_end tl tls local_vars global_vars func_lst (pop(nested_stmts))
        |"Call" ->
          (match tls with
          |[] -> (parse_list, String "Error", global_vars) 
          |hd::[] -> (parse_list, String "Error", global_vars)
          |hd::hd2::tl2 ->
            match hd with
            |Int _ |String _ |Tuple _|Union _ -> (parse_list, String "Error", global_vars)
            |Clo(func_name, var_name, func_cmds, is_mut) -> let func_result = perform_func_cmds func_cmds func_name is_mut [] (store_func_vars var_name hd2 local_vars) global_vars func_lst in 
            match func_result with
            |(func_val, new_global_vars) ->
            if(func_val = String("Error")) then
              (parse_list, String "Error", global_vars)
            else
              begin_end tl (push func_val tl2) local_vars global_vars func_lst nested_stmts)
    |"Quit" -> (tl, String("Error"), global_vars)
    |_ ->
      if(String.sub hd (0) (3) = "Fun") then
        let func_name = String.split_on_char(' ') (String.sub hd 4 ((String.length hd)-4)) in
        let func_cmds = get_func_cmds tl 0 in
        (match func_cmds with
        |(fun_cmds, parse_tl, mut_cmds) -> 
          if(List.hd fun_cmds = "Error") then
            (parse_list, String "Error", global_vars)
        else
          (match func_name with
          |[] -> (parse_list, String "Error", global_vars)
          |_::[] -> (parse_list, String "Error", global_vars)
          |fun_name::fun_var::tl -> 
              begin_end parse_tl (tls) local_vars global_vars (Clo(fun_name, fun_var, fun_cmds, false)::preserve_funcs (mut_cmds) func_lst) nested_stmts )) 
      else if(String.sub (hd) (0) (5) = "Tuple") then
        if((int_of_string (String.sub (hd) (6) ((String.length hd)-6))) <= List.length tls) then
          let tuple_result = create_tuple (int_of_string (String.sub (hd) (6) ((String.length hd)-6))) (tls) in
          (match tuple_result with 
          |(tuple_ls, sls_tl) -> 
            if(List.hd tls = String "Error") then
              (parse_list, String("Error"), global_vars)
          else 
            begin_end tl (push (Tuple(tuple_ls)) tls) local_vars global_vars func_lst nested_stmts
          )
        else
          (parse_list, String("Error"), global_vars)
      else if(String.sub (hd) (0) (3) = "Get") then
        if(List.length tls < 1 || (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) >= List.length tls) then
          (parse_list, String("Error"), global_vars)
        else
          (match List.hd tls with
          |String _ -> (parse_list, String("Error"), global_vars)
          |Int _ -> (parse_list, String("Error"), global_vars)
          |Union _ ->(parse_list, String("Error"), global_vars)
          |Clo _ -> (parse_list, String("Error"), global_vars)
          |Tuple(value) -> let get_result = get_tuple_val 0 (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) (Tuple(value)) in 
          if(get_result = String("Error")) then
            (parse_list, String("Error"), global_vars)
          else
            begin_end tl (push get_result tls) local_vars global_vars func_lst nested_stmts)
      else if(String.sub hd 0 5 = "Local") then 
          if(List.length tls < 1) then
            (tl, String("Error"), global_vars)
          else if(String.length hd = 7 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            begin_end tl (pop(tls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (tls) (local_vars)) global_vars func_lst nested_stmts
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            begin_end tl (pop(tls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (tls) (local_vars)) global_vars func_lst nested_stmts
          else
            (tl, String("Error"), global_vars) 
        else if(String.sub hd 0 6 = "Global") then
          if(List.length tls < 1) then 
            (tl, String("Error"), global_vars)
          else if(String.length hd = 8 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            begin_end tl (pop(tls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (tls) (global_vars)) func_lst nested_stmts
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            begin_end tl (pop(tls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (tls) (global_vars)) func_lst nested_stmts
          else
            (tl, String("Error"), global_vars)
        else if(String.sub hd (0) (4) = "Push") then
          if(is_var (String.sub hd 5 ((String.length hd)-5)) local_vars) then
            let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) local_vars) (tls) in 
              begin_end tl push_result local_vars global_vars func_lst nested_stmts
          else if(is_var (String.sub hd 5 ((String.length hd)-5)) global_vars) then
            let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) global_vars) (tls) in 
            begin_end tl push_result local_vars global_vars func_lst nested_stmts
          else if(Str.string_match(Str.regexp "[0-9|-0--9]+$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
           let push_result = push (Int(int_of_string(String.sub (hd) (5) ((String.length hd)-5)))) tls in
            begin_end tl push_result local_vars global_vars func_lst nested_stmts
          else if(Str.string_match(Str.regexp "\"[a-z|A-Z]+\"$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
            let push_result = push (String((String.sub (hd) (5) ((String.length hd)-5)))) tls in
            begin_end tl push_result local_vars global_vars func_lst nested_stmts
          else
            (tl, String("Error"), global_vars)
        else
          (tl, String("Error"), global_vars)

let rec execute_commands (parse_list: string list) (sls: stack_val list) (local_vars: (string*stack_val) list) 
(global_vars: (string*stack_val) list) (func_list: stack_val list) (file_path: string) = 
  match parse_list with
  |[] -> empty file_path
  |hd::tl ->
    match hd with
    |"Pop" ->  
      if(sls_count sls < 1) then
        error file_path
    else 
      let pop_result = pop sls in 
      execute_commands tl pop_result local_vars global_vars func_list file_path
    |"Add" -> let add_result = add sls in 
      if(add_result = String("Error")) then
        error file_path
      else
        execute_commands tl (push (add_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Sub" -> let sub_result = sub sls in 
      if(sub_result = String("Error")) then
        error file_path
      else
        execute_commands tl (push (sub_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Mul" -> let mul_result = mul sls in 
      if(mul_result = String("Error")) then
        error file_path
      else
        execute_commands tl (push (mul_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Div" -> let div_result = div sls in 
      if(div_result = String("Error")) then
        error file_path
      else
        execute_commands tl (push (div_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Swap" -> let swap_result = swap sls in
      if(List.hd swap_result = String("Error")) then
        error file_path
      else 
        execute_commands tl swap_result local_vars global_vars func_list file_path
    |"Neg" -> let neg_result = neg sls in
      if(neg_result = String("Error")) then
        error file_path
      else 
        execute_commands tl (push (neg_result) (pop sls)) local_vars global_vars func_list file_path
    |"Concat" -> let concat_result = concat sls in 
      if(concat_result = String("Error")) then
        error file_path
      else 
        execute_commands tl (push (concat_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"And" -> let and_result = and_func sls in 
        if(and_result = String("Error")) then
          error file_path
      else
        execute_commands tl (push (and_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Or" -> let or_result = or_func sls in
        if(or_result = String("Error")) then
          error file_path
      else
        execute_commands tl (push (or_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Lte" -> let lte_result = lte sls in 
        if(lte_result = String("Error")) then
          error file_path
        else
          execute_commands tl (push (lte_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Equal" -> let equal_result = equal sls in
        if(equal_result = String("Error")) then
          error file_path
        else
          execute_commands tl (push (equal_result) (pop(pop sls))) local_vars global_vars func_list file_path
    |"Not" -> let not_result = not sls in 
        if(not_result = String("Error")) then
          error file_path
        else
          execute_commands tl (push (not_result) (pop(sls))) local_vars global_vars func_list file_path
    |"IfThen" -> let ifthen_result = ifthen sls in
          if(ifthen_result = String("Error")) then
            error file_path
          else if(ifthen_result = Int(1)) then
            execute_commands tl (pop sls) local_vars global_vars func_list file_path
          else
            execute_commands (skip_ifthen_commands tl) (pop sls) local_vars global_vars func_list file_path
    |"Else" -> execute_commands (skip_else_commands tl) (sls) local_vars global_vars func_list file_path
    |"End" -> execute_commands tl sls local_vars global_vars func_list file_path
    |"Begin" -> let begin_result = begin_end tl [] local_vars global_vars func_list [] in
      (match begin_result with
      |(parse_tl, hd, global_var_lst)->
        if(hd = String("Error")) then
          error file_path
        else
          execute_commands parse_tl (push hd sls) local_vars global_var_lst func_list file_path)
    |"InjL" -> let injl_res = left_inj sls in
      if(injl_res = String("Error")) then
        error file_path
      else 
        execute_commands tl (push (injl_res) (pop(sls))) local_vars global_vars func_list file_path
    |"InjR" -> let injr_res = right_inj sls in
      if(injr_res = String("Error")) then
        error file_path
      else
        execute_commands tl (push (injr_res) (pop(sls))) local_vars global_vars func_list file_path
    |"CaseLeft" -> let caseleft_res = caseleft sls in
        (match caseleft_res with
        |(bool_check, value) -> 
          if(value = String "Error") then
            error file_path
        else if(bool_check = Int 0) then
          execute_commands (skip_caseleft tl 0) (push (right(sls)) (pop(sls))) local_vars global_vars func_list file_path
        else 
          execute_commands tl (push value (pop (sls))) local_vars global_vars func_list file_path)
    |"Right" -> execute_commands (skip_right tl 0) sls local_vars global_vars func_list file_path
    |"Return" -> error file_path
    |"Call" ->
      (match sls with
      |[] -> error file_path 
      |hd::[] -> error file_path
      |hd::hd2::tl2 ->
        match hd with
        |Int _ |String _ |Tuple _|Union _ -> error file_path
        |Clo(func_name, var_name, func_cmds, is_mut) -> let func_result = perform_func_cmds func_cmds func_name is_mut [] (store_func_vars var_name hd2 local_vars) global_vars func_list in 
        match func_result with
        |(func_val, new_global_vars) ->
        if(func_val = String("Error")) then
          error file_path
        else
          execute_commands tl (push func_val tl2) local_vars global_vars func_list file_path)
    |"Quit" -> write_file_parse sls file_path
    |_ ->
      (*This is where we check to see if the command is either Push, Local, Global, or Tuple*)
      if(String.sub hd (0) (3) = "Fun") then
        let func_name = String.split_on_char(' ') (String.sub hd 4 ((String.length hd)-4)) in
        let func_cmds = get_func_cmds tl 0 in
        (match func_cmds with
        |(fun_cmds, parse_tl, mut_cmds) -> 
          if(List.hd fun_cmds = "Error") then
            error file_path
        else
          (match func_name with
          |[] -> error file_path
          |_::[] -> error file_path
          |fun_name::fun_var::tl -> 
              execute_commands parse_tl (sls) local_vars global_vars (Clo(fun_name, fun_var, fun_cmds, false)::preserve_funcs (mut_cmds) func_list) file_path ))
      else if(String.sub (hd) (0) (5) = "Tuple") then
        if((int_of_string (String.sub (hd) (6) ((String.length hd)-6))) <= List.length sls) then
          let tuple_result = create_tuple (int_of_string (String.sub (hd) (6) ((String.length hd)-6))) (sls) in
          (match tuple_result with 
          |(tuple_ls, sls_tl) -> 
          (match sls_tl with
            |[] -> execute_commands tl (push (Tuple(tuple_ls)) sls_tl) local_vars global_vars func_list file_path
            |String "Error" :: _ -> error file_path
            |hd::tl2 -> execute_commands tl (push (Tuple(tuple_ls)) sls_tl) local_vars global_vars func_list file_path))
        else
          error file_path 
      else if(String.sub (hd) (0) (3) = "Get") then
        if(List.length sls < 1 || (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) >= List.length sls) then
          error file_path
        else
          (match List.hd sls with
          |String _ -> error file_path
          |Int _ -> error file_path
          |Union _ -> error file_path
          |Clo _ -> error file_path
          |Tuple(value) -> let get_result = get_tuple_val 0 (int_of_string (String.sub (hd) (4) ((String.length hd)-4))) (Tuple(value)) in 
          if(get_result = String("Error")) then
            error file_path
          else
            execute_commands tl (push get_result sls) local_vars global_vars func_list file_path)
      else if(String.sub (hd) (0) (4) = "Push") then
        if (is_func (String.sub hd (5) ((String.length hd)-5)) func_list) then
          let push_result = push (get_func (String.sub hd (5) ((String.length hd)-5)) (func_list)) sls in
          execute_commands tl push_result local_vars global_vars func_list file_path
      else if(is_var(String.sub hd 5 ((String.length hd)-5)) local_vars) then
          let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) local_vars) sls in
          execute_commands tl push_result local_vars global_vars func_list file_path
      else if(is_var(String.sub hd 5 ((String.length hd)-5)) global_vars) then
        let push_result = push (get_var (String.sub hd 5 ((String.length hd)-5)) global_vars) sls in
        execute_commands tl push_result local_vars global_vars func_list file_path
      else if(Str.string_match(Str.regexp "[0-9|-0--9]+$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
         let push_result = push (Int(int_of_string(String.sub (hd) (5) ((String.length hd)-5)))) sls in
          execute_commands tl push_result local_vars global_vars func_list file_path
        else if(Str.string_match(Str.regexp "\"[a-z|A-Z]+\"$") (String.sub (hd) (5) ((String.length hd)-5)) 0) then
          let push_result = push (String((String.sub (hd) (5) ((String.length hd)-5)))) sls in
          execute_commands tl push_result local_vars global_vars func_list file_path
        else 
          error file_path
        else if(String.sub hd 0 5 = "Local") then 
          if(List.length sls < 1) then
            error file_path
          else if(String.length hd = 7 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            execute_commands tl (pop(sls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (sls) (local_vars)) global_vars func_list file_path
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (6) ((String.length hd)-6)) 0) then
            execute_commands tl (pop(sls)) (store_vars (String.sub hd 6 ((String.length hd)-6)) (sls) (local_vars)) global_vars func_list file_path
          else
            error file_path
        else if(String.sub hd 0 6 = "Global") then
          if(List.length sls < 1) then 
            error file_path
          else if(String.length hd = 8 && Str.string_match(Str.regexp "[a-z]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            execute_commands tl (pop(sls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (sls) (global_vars)) func_list file_path
          else if(Str.string_match(Str.regexp "[a-z][a-z|A-Z|_|0-9]+$") (String.sub (hd) (7) ((String.length hd)-7)) 0) then
            execute_commands tl (pop(sls)) local_vars (store_vars (String.sub hd 7 ((String.length hd)-7)) (sls) (global_vars)) func_list file_path
          else
            error file_path
        else
          error file_path

(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)

let test1 = "Fun filter x
Push x
Get 0
CaseLeft
Get 1
Global old
Get 0
Local x
Push x
Push five
Call
Return
Right
Push new
Return
End
Mut five x
Push x
Push 5
Equal
IfThen
Push x
Push new
Tuple 2
InjL
Global new
Push old
Push new
Tuple 2
Push filter
Call
Return
Else
Push old
Push new
Tuple 2
Push filter
Call
Return
End
End
Push filter
Call
Quit"
let interpreter (src : string) (output_file_path: string): unit =
  execute_commands (parse src) [] [] [] [] output_file_path
  