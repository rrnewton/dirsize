

val expand_dir :
  string ->
  (string * Unix.stats) list * (string * Unix.stats) list *
  (string * Unix.stats) list


  (** There are three types of filetrees.  Plain filetrees are
    straightforward.  lfiltrees are lazy, hence subdisks are not read
    from the hard disk before they are required.  Also, lfiletrees have
    file statistic information as well as file names. 

    Plain filetrees are meant to be painfully simple and obvious; this
    is why they include no statistic information.  Lfiletrees are more
    advanced.

    The third type are afiletrees.  They have are annotated with full
    path information for each node, which makes them a good choice for
    when one actually wants to operate on files on disk. *)
type filetree =
    Dir  of string * filetree list
  | Link of string * string
  | File of string
and lfiletree =
    Ldir  of (string * Unix.stats) * lfiletree list Lazy.t
  | Llink of (string * Unix.stats) * string
  | Lfile of (string * Unix.stats)
and afiletree =
    Adir  of (string * Unix.stats * string) * lfiletree list Lazy.t
  | Alink of (string * Unix.stats * string) * string
  | Afile of (string * Unix.stats * string)

val force_ltree : lfiletree -> unit
val tree_of_ltree : lfiletree -> filetree

(*val atree_of_ltree : lfiletree -> filetree*)

val read_ldir : string -> lfiletree
val read_dir : string -> filetree

val print_tree : filetree -> unit

type pair = int * int
val dircount_old : lfiletree -> pair * pair * pair

type dircount_t = { mutable files: int; 
		    mutable filebytes: int;
		    mutable dirs: int; 
		    mutable dirbytes: int;
		    mutable links: int; 
		    mutable linkbytes: int }

val dircount : lfiletree -> dircount_t
