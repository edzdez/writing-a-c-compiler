open! Core

module Declaration = struct
  type t =
    | Declared of string
    | Duplicate
end

type t =
  { vars : string String.Table.t
  ; fresh_ids : Common.Fresh_id.t
  }

let create () = { vars = String.Table.create (); fresh_ids = Common.Fresh_id.create () }

let fresh_variable t ~source_name =
  [%string "var.%{source_name}.%{Common.Fresh_id.fresh t.fresh_ids#Int}"]
;;

let declare t ~source_name =
  match Hashtbl.mem t.vars source_name with
  | true -> Declaration.Duplicate
  | false ->
    let resolved_name = fresh_variable t ~source_name in
    Hashtbl.add_exn t.vars ~key:source_name ~data:resolved_name;
    Declared resolved_name
;;

let find t source_name = Hashtbl.find_or_null t.vars source_name
