type range = { low : int; high : int; }
val create_opt : int -> int -> range option
val create : int -> int -> range

val low : range -> int
val high : range -> int

val length : range -> int

val int_list_of_range : range -> int list

val get_overlap_opt : range -> range -> range option

exception RangeDecreases
