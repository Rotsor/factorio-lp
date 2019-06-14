type t
val tag : string -> t -> t
val tag_multi : string -> t list -> t
val table : t list -> t
val tr : t list -> t
val th : t -> t
val td : t -> t
val text : string -> t
val concat : t list -> t

val link : url:string -> t -> t
val anchor : id:string -> t -> t


val render : t -> string

val div : ?class_:string -> t -> t

val img : url:string -> t
