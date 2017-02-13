type t = {mutable current_file : string; mutable next_regex : unit -> (int * string) option; mutable file_stack : string list};;

val make : string -> t;;

val next : t -> (string * int * string) option;;
