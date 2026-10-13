open! Core

type t

val create : unit -> t

val declare_local_var
  :  t
  -> source_name:string
  -> has_linkage:bool
  -> [ `Declared of string | `Duplicate ]

val declare_file_scope_var : t -> source_name:string -> unit
val declare_fun : t -> source_name:string -> [ `Declared of string | `Duplicate ]
val find : t -> string -> string or_null
val with_scope : t -> f:(unit -> 'a) @ local -> 'a
