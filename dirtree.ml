open Int64
open Printf
open Unix
open Unix.LargeFile
open Utils
open Utils.RMisc
open Filename

(*let string 
let comma_str s = 
  let chop_3 s = 
    let len = String.length s in
      if len < 3 then s
      else (String.sub s 0 3, String.sub s 3 len)
  
*)


(*let rec list_chop n ls = 
  function
      0  ls     -> ([],ls)
    | n  h::t   -> let frnt,back = loop (n-1) t in (h::frnt,back)
    | n  []     -> raise End_of_file   *)


(*******************************************************)      

let expand_dir path = 
  let dir = opendir path in
  let regacc = ref [] 
  and diracc = ref []
  and symacc = ref [] in
    try 
      while true do
	let s = readdir dir in
	  if s <> "." && s <> ".."
	  then
(*	    if s = ".DRM_etc" || s = ".DRM_Home" || 
	       s=".DS_Store"  || s = ".hidden"
	    then eprintf "HMM hit a %s\n" s
	    else*) 
	    let stats = lstat (concat path s) in
	      match stats.st_kind  with
		  S_REG -> regacc := (s,stats) :: !regacc
		| S_DIR -> diracc := (s,stats) :: !diracc
		| S_LNK -> symacc := (s,stats) :: !symacc
		| _ -> failwith "hmm, bad file kind..." 
      done; 
      ([],[],[])
    with 
	End_of_file -> 
	  (closedir dir;
	   (List.rev !regacc, 
	    List.rev !diracc, 
	    List.rev !symacc))
	  
(********************************************************)



(* These type abbreviations are cyclic... sigh *)
(*type 'a general_filetree =
    Dir of string * 'a
  | File of string 
  | Link of string * string
type filetree = (filetree list) general_filetree
type lfiletree = (lfiletree list Lazy.t) general_filetree
type pfiletree = ((pfiletree * stats) list Lazy.t) general_filetree*)

type filetree = 
    Dir of  string * filetree list
  | Link of string * string
  | File of string 

and lfiletree =
    Ldir of  (string * stats) * lfiletree list Lazy.t
  | Llink of (string * stats) * string
  | Lfile of (string * stats)

and afiletree =
    Adir of  (string * stats * string) * lfiletree list Lazy.t
  | Alink of (string * stats * string) * string
  | Afile of (string * stats * string)


let rec force_ltree ltree = 
  match ltree with 
      Lfile _ -> ()     
    | Llink _ -> ()
    | Ldir (_,ltree) -> 
	List.iter force_ltree (Lazy.force ltree)

let rec tree_of_ltree ltree = 
  match ltree with 
      Lfile ( name,_)         -> File name
    | Llink ((name,_),link)  -> Link (name,link)
    | Ldir  ((name,_),ltree) -> 
	Dir (name,
	     List.map tree_of_ltree (Lazy.force ltree))

(* This should really be called read_lfile, because it can read
   symlinks and plainfiles as well as directories... *)
let rec read_ldir path =
  let stats = lstat path in
    match stats.st_kind  with
	S_REG -> Lfile (basename path,stats)
      | S_LNK -> Llink ((basename path,stats),readlink path)
      | S_DIR -> 
	  let rec loop name path stats = 
	    Ldir 
	      ((name,stats),
	       lazy
		 (let files,dirs,syms = expand_dir path in
		    List.map (fun x -> Lfile x) files @
		    List.map (fun (n,s) -> 
				Llink ((n,s),(readlink (concat path n)))) 
		      syms @
		    List.map (fun (n,s) -> 
				loop n (concat path n) s) dirs)) in 
	    loop (basename path) path (lstat path)
      | _ -> failwith 
	  (sprintf "read_ldir: this file is of an unknown type: %s" path)

(* Hmm I just put an 'name' in place of the loop 'n' above...  Are
   there any extrinsic properties of the program that could hint at
   that error *)

let read_dir path = tree_of_ltree (read_ldir path)

let print_tree ft =
  let rec loop indent tree = 
    match tree with 
	File s -> printf "%s%s\n" indent s
      | Link (s,l) -> printf "%s%s -> %s\n" indent s l
      | Dir (s,ls) ->
	  printf "%s%s:\n" indent s;
	  List.iter (loop (indent^"  ")) ls
  in loop "" ft

(**********************************************************************)
(* I have two versions of dircount here... one returns a bunch of
   tuples, the other returns a struct *)

(*type pair = int * int

(** This takes a lazy filtree and returns a triplet of doubletons, the
  doubletons represent the number of files and their size in bytes, the
  number of directories and their size in bytes, and the number of
  symlinks and their size in bytes. *)
let rec dircount_old ltree = 
  match ltree with 
      Lfile (_,s)      -> ((1,s.st_size), (0,0), (0,0))
    | Llink ((_,s),_)  -> ((0,0), (0,0), (1,s.st_size))
    | Ldir  ((_,s),lazyls) ->
	List.fold_left
	(fun ((f,fs), (d,ds), (l,ls)) tree ->
	   let ((f',fs'), (d',ds'), (l',ls')) = dircount_old tree in
	     ((f+f',fs+fs'),
	      (d+d',ds+ds'),
	      (l+l',ls+ls')))
	((0,0), (1,s.st_size), (0,0)) 
	(Lazy.force lazyls)
*)



type dircount_t = { mutable files:     int; 
		    mutable filebytes: Int64.t;
		    mutable dirs:      int; 
		    mutable dirbytes:  Int64.t;
		    mutable links:     int; 
		    mutable linkbytes: Int64.t }

let dircount ltree = 
  let sum = { files = 0;  filebytes = zero;
	      dirs  = 0;  dirbytes  = zero;
	      links = 0;  linkbytes = zero } in
    (* We print a progress indicator every 500 files. Hence the counter. *)
  let rec loop trees counter =
(*	    printf "\n trees on stack: %d\n" (List.length trees);
	    printf "MEM alloced: %f\n" (Gc.allocated_bytes ());
	    (*Gc.print_stat Pervasives.stdout;*)
	    Gc.full_major ();
	    printf "GC DONE\n";*)
    let counter = 
      if counter > 500
      then (print_char '.';
	    Pervasives.flush Pervasives.stdout;	    
	    counter - 499)
      else counter + 1
    in
      if trees = []
      then sum
      else match List.hd trees with 
	  Lfile (_,s) ->
	    sum.files <- sum.files + 1; 
	    sum.filebytes <- add sum.filebytes s.st_size;
	    loop (List.tl trees) counter
	      
	| Llink ((_,s),_) -> 
	    sum.links <- sum.links + 1; 
	    sum.linkbytes <- add sum.linkbytes s.st_size;
	    loop (List.tl trees) counter
	      
	| Ldir  ((_,s),lazyls) ->
	    sum.dirs <- sum.dirs + 1;
	    sum.dirbytes <- add sum.dirbytes s.st_size;
	    loop (Lazy.force lazyls @ List.tl trees) counter
  in loop [ ltree ] 0

(*
let dircount2 ltree = 
  let rec loop (trees:lfiletree list) sum =
    if trees = []
    then sum
    else match List.hd ltree with 
	Lfile (_,s) ->
	  loop (List.tl trees)
	  { sum with 
	      files = sum.files + 1; 
	      filebytes = sum.filebytes + s.st_size }

      | Llink ((_,s),_) -> 
	  loop (List.tl trees)
	  { sum with 
	      links = sum.links + 1; 
	      linkbytes = sum.linkbytes + s.st_size }

      | Ldir  ((_,s),lazyls) ->
	  loop (Lazy.force lazyls @ List.tl trees) 
	  { sum with 
	      dirs = sum.dirs + 1;
	      dirbytes = sum.dirbytes + s.st_size }
  in loop [ltree] 
       { files = 0;  filebytes = 0;
	 dirs  = 0;  dirbytes  = 0;
	 links = 0;  linkbytes = 0 }
*)
       

