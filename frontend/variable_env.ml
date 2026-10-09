open! Core

type t =
  { mutable vars : string String.Table.t list
  ; fresh_ids : Common.Fresh_id.t
  }

let create_layer () = String.Table.create ()
let create () = { vars = [ create_layer () ]; fresh_ids = Common.Fresh_id.create () }

let fresh_variable t ~source_name =
  [%string "var.%{source_name}.%{Common.Fresh_id.fresh t.fresh_ids#Int}"]
;;

let declare t ~source_name =
  match t.vars with
  | [] -> raise_s [%message "no open scopes"]
  | vars :: _ ->
    (match Hashtbl.mem vars source_name with
     | true -> `Duplicate
     | false ->
       let resolved_name = fresh_variable t ~source_name in
       Hashtbl.add_exn vars ~key:source_name ~data:resolved_name;
       `Declared resolved_name)
;;

let find t source_name =
  let rec local_ go = function
    | [] -> Null
    | h :: t ->
      (match Hashtbl.find_or_null h source_name with
       | This v -> This v
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
