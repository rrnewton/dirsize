

(** This expands a directory into a triple: (plainfiles, dirs, symlinks) *)
val expand_dir :
  string ->
  (string * Unix.LargeFile.stats) list * 
  (string * Unix.LargeFile.stats) list *
  (string * Unix.LargeFile.stats) list


  (** There are three types of filetrees.  Plain filetrees are
    straightforward.  lfiltrees are lazy, hence subdisks are not read
    from the hard disk before they are required.  Also, lfiletrees have
    file statistic information as well as file names. 

    Plain filetrees are meant to be painfully simple and obvious; this
    is why they include no stats information.  Lfiletrees are more
    advanced.

    The third type are afiletrees.  They have are annotated with full
    path information for each node, which makes them a good choice for
    when one actually wants to operate on files on disk. *)
type filetree =
    Dir  of string * filetree list
  | Link of string * string
  | File of string
and lfiletree =
    Ldir  of (string * Unix.LargeFile.stats) * lfiletree list Lazy.t
  | Llink of (string * Unix.LargeFile.stats) * string
  | Lfile of (string * Unix.LargeFile.stats)
and afiletree =
    Adir  of (string * Unix.LargeFile.stats * string) * lfiletree list Lazy.t
  | Alink of (string * Unix.LargeFile.stats * string) * string
  | Afile of (string * Unix.LargeFile.stats * string)
      (** The triple in each of these variants contains (name, stats,
	fullqpath) *)

(** NOTE - I HAVENT ACTUALLY IMPLEMENTED AFILETREES YET. *)

val force_ltree : lfiletree -> unit
  (** Force_ltree.  This fully evaluates all the subdirs, reading all
    relevent information of the hard drive. *)
val tree_of_ltree : lfiletree -> filetree
  (** Tree_of_ltree does the same thing as force_ltree, but also
    rebuilds the no-longer-lazy tree as a normal filetree. *)

(*val atree_of_ltree : lfiletree -> filetree*)

val read_ldir : string -> lfiletree
val read_dir : string -> filetree

val print_tree : filetree -> unit


(*type pair = int * int
val dircount_old : lfiletree -> pair * pair * pair*)


type dircount_t = { mutable files:     int; 
		    mutable filebytes: Int64.t;
		    mutable dirs:      int; 
		    mutable dirbytes:  Int64.t;
		    mutable links:     int; 
		    mutable linkbytes: Int64.t }

val dircount : lfiletree -> dircount_t
  (** Dircount counts the number of files of different types within a
    directory and all its subdirectories.  As a bonus we get
    (system-independent) byte counts. *)
