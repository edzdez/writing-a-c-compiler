open! Core

type t =
  { mutable vars : (name:string * has_linkage:bool) String.Table.t list
  ; fresh_ids : Common.Fresh_id.t
  }

let create_layer () = String.Table.create ()
let create () = { vars = [ create_layer () ]; fresh_ids = Common.Fresh_id.create () }

let fresh_variable t ~source_name =
  [%string "var.%{source_name}.%{Common.Fresh_id.fresh t.fresh_ids#Int}"]
;;

let declare_local_var t ~source_name ~has_linkage =
  match t.vars with
  | [] -> raise_s [%message "no open scopes"]
  | vars :: _ ->
    (match Hashtbl.find_or_null vars source_name with
     | This (~name:_, ~has_linkage:prev_has_linkage)
       when not (prev_has_linkage && has_linkage) -> `Duplicate
     | This _ | Null ->
       let name =
         match has_linkage with
         | true -> source_name
         | false -> fresh_variable t ~source_name
       in
       Hashtbl.set vars ~key:source_name ~data:(~name, ~has_linkage);
       `Declared name)
;;

let declare_file_scope_var t ~source_name =
  match t.vars with
  | [] -> raise_s [%message "no open scopes"]
  | vars :: _ ->
    Hashtbl.set vars ~key:source_name ~data:(~name:source_name, ~has_linkage:true)
;;

let declare_fun t ~source_name = declare_local_var t ~source_name ~has_linkage:true

let find t source_name =
  let rec local_ go = function
    | [] -> Null
    | h :: t ->
      (match Hashtbl.find_or_null h source_name with
       | This (~name, ~has_linkage:_) -> This name
       | Null -> go t)
  in
  go t.vars [@nontail]
;;

let open_scope t = t.vars <- create_layer () :: t.vars

let close_scope t =
  t.vars
  <- (match t.vars with
      | [] -> raise_s [%message "no open scopes"]
      | _ :: vars -> vars)
;;

let with_scope t ~(f @ local) =
  open_scope t;
  Exn.protect ~f ~finally:(fun () -> close_scope t)
;;
