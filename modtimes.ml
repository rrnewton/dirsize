(** modtimes.ml

  This program makes use of my little Dirtree library to list earliest
  and latest modification times.
 
  --Ryan Newton
*)

open Printf
open Graphs
open Rutils
open Unix
open Unix.LargeFile
open Dirtree

let use_color   = ref false
let norm_color  = ref Ansi.Darkgray
let path_color  = ref Ansi.Red
let total_color = ref Ansi.Green
let count_color = ref Ansi.Lightgreen

let verbose = ref true 

(** Print date in year-first format.*)
let year_first = ref false

(** Print average dates rather than earliest and latest. *)
let averages = ref false

(** Graphs a distribution of times. *)
let graph_distro = ref false

(*let sort_output = ref false*)


(********************************************************************)
module Localutils = 
struct
  let comma_str s = 
    let ls = RString.charlist_of_string s in
    let sep = RList.chunk 3 (List.rev ls) in
    let flipped = List.rev (List.map List.rev sep) in
    let strs = List.map RString.string_of_charlist flipped in    
      String.concat "," strs
	
  let comma_int i = comma_str (string_of_int i)
  let comma_int64 i = comma_str (Int64.to_string i)
			
  let switch_color c = 
    if !use_color
    then Ansi.switch_color c    
end
open Localutils
(********************************************************************)

type span = { early : float;
	      acc   : float list;
	      sum   : float; 
	      count : float;
	      late  : float }

type modtimes_t = { mutable filespan : span;
		    mutable dirspan  : span; 
		    mutable linkspan : span }

let base_span = { early=max_float ; 
		  acc   = [];
		  sum   = 0.0;
		  count = 0.0;
		  late=min_float }

let is_live span = 
  ((span.early <> max_float) || 
   (span.late  <> min_float) || 
   (span.count > 0.0))

let update_span spn time =
  let early = (if time < spn.early
	       then time
	       else spn.early)
  and late  = (if time > spn.late
	       then time 
	       else spn.late) 
  and acc   = time :: spn.acc
  and sum   = spn.sum +. time
  and count = spn.count +. 1.0  in
    { early= early; 
      acc= acc;
      sum= sum;
      count= count;
      late= late }

open Int64;;
    
let zed = { filespan = base_span;
	    dirspan  = base_span;
	    linkspan = base_span }

let scan_modtimes ?(print_progress=false) ltree = 
  let sum = zed in
    (* We print a progress indicator every 500 files. Hence the counter. *)
    (* This is written so that it SHOULD do a depth first traversal
       of the tree without building the whole thing in memory.  It
       would do well to verify this property. *)
  let rec loop trees counter =
    (*	    printf "\n trees on stack: %d\n" (List.length trees);
	    printf "MEM alloced: %f\n" (Gc.allocated_bytes ());
    (*Gc.print_stat Pervasives.stdout;*)
	    Gc.full_major ();
	    printf "GC DONE\n";*)
    let counter = 
      if counter > 500 && print_progress
      then (print_char '.';
	    Pervasives.flush Pervasives.stdout;	    
	    counter - 499)
      else counter + 1
    in
      if trees = []
      then sum
      else (match List.hd trees with 
		Lfile (_,s) ->
		  sum.filespan <- update_span sum.filespan s.st_mtime;
		  loop (List.tl trees) counter
		    
	      | Llink ((_,s),_) -> 
		  sum.linkspan <- update_span sum.linkspan s.st_mtime;
		  loop (List.tl trees) counter
		    
	      | Ldir  ((_,s),lazyls) ->
		  sum.dirspan <-  update_span sum.dirspan  s.st_mtime;
		  loop (Lazy.force lazyls @ List.tl trees) counter)

  in loop [ ltree ] 0


let graph_it sum = 
  histogram (Array.of_list sum.filespan.acc)

let print_date date = 
  if date = min_float ||  date = max_float 
  then "_"
  else 
    let date = localtime date in
      if not !year_first
      then sprintf "%02d/%02d/%04d" 
	(date.tm_mon+1) 
	(date.tm_mday)
	(date.tm_year+1900)
      else sprintf "%02d.%02d.%02d" 
	(date.tm_year+1900)
	(date.tm_mon+1) 
	(date.tm_mday)

let print_span spn = 
  sprintf "from %s to %s" 
    (print_date spn.early) 
    (print_date spn.late) 
 
let print_averages spn = 
  sprintf "average date: %s" 
    (print_date (spn.sum /. spn.count))

let print_res indent (sum : modtimes_t) =
  if !verbose
  then 
    let helper printer = 
      (if is_live sum.filespan
       then printf "%sFiles %s.\n" indent (printer sum.filespan));
      (if is_live sum.dirspan
       then printf "%sDirs  %s.\n" indent (printer sum.dirspan));
      (if is_live sum.linkspan
       then printf "%sLinks %s.\n" indent (printer sum.linkspan)); 
    in
      if !averages
      then helper print_averages
      else helper print_span;    
      if !graph_distro then graph_it sum;;

	  
let plus_span a b = 
  { early = min a.early b.early;
    acc   = a.acc @ b.acc;
    sum   = a.sum +. b.sum;
    count = a.count +. b.count;
    late  = max a.late  b.late; };;

let plus x y =
  { filespan = plus_span x.filespan y.filespan;
    dirspan  = plus_span x.dirspan  y.dirspan;
    linkspan = plus_span x.linkspan y.linkspan; };;

(*    if !use_color
    then
      Ansi.print_colored 
	[ !total_color, (sprintf "%s%14s" indent (comma_int64 sum.filebytes));
	  !norm_color, " bytes in ";
	  !count_color, (string_of_int sum.files);
	  !norm_color, " plain files.\n";
	  
	  !norm_color, sprintf "%s%14s bytes in " 
	    indent (comma_int64 sum.dirbytes);
	  !count_color, (string_of_int sum.dirs);
	  !norm_color, " directories.\n";

	  !norm_color, sprintf "%s%14s bytes in " 
	    indent (comma_int64 sum.linkbytes);
	  !count_color, (string_of_int sum.links);
	  !norm_color, " symlink files.\n";
	  
	  !norm_color, sprintf "%s%14s bytes total.\n" 
	    indent 
	    (comma_int64 (add (add sum.filebytes sum.dirbytes) sum.linkbytes))
	]
    else
      (printf "%s%14s bytes in %d plain files.\n" 
	 indent (comma_int64 sum.filebytes) sum.files;
       printf "%s%14s bytes in %d directories.\n" 
	 indent (comma_int64 sum.dirbytes) sum.dirs;
       printf "%s%14s bytes in %d symlink files.\n" 
	 indent (comma_int64 sum.linkbytes) sum.links;
       printf "%s%14s bytes total.\n" 
	 indent (comma_int64
		   (add (add sum.filebytes sum.dirbytes) sum.linkbytes )));
*)


(********************************************************************)


let print_help () = 
  print_endline "Usage: mods <options> <path-names>";
  print_endline "options are:";
  print_endline "  -h   Show this help message.";
  print_endline "  -c   Use ANSI color.  Green and red theme.";
  print_endline "  -b   Use ANSI color.  Blue and yellow theme.";
  print_endline "  -nc  Don't use color.  Plain ASCII.";
  print_endline "  -s   Sort output by filesize, increasing.";
  exit 0;;

let pair a b = (a,b) 

(********************************************************************)
(* Main script *)

let main () =

  let paths = (* Here are all those filepaths. *)
    (let temp = List.filter
		  (* Process flags *)
		  (function
		     | "-h"  -> print_help (); false
		     | "-av"  -> averages := true; false
		     | "-g"  -> graph_distro := true; false

		     | "-c"  -> use_color := true; false
		     | "-nc" -> use_color := false; false
		     | "-b"  -> use_color := true; 
			 path_color  := Ansi.Lightblue;
			 total_color := Ansi.Yellow;
			 false
			   (*		     | "-s"  -> sort_output := true; false  *)
		     | _     -> true)
		  (List.tl (Array.to_list Sys.argv)) in
       if temp = []
	 (* DEBUGGING: *) 
	 (*then ["/ffh/ryan/home"]*)
       then [Filename.current_dir_name]
       else temp) in
    
  let counts = 
    List.map
      (fun path ->
	 (if !use_color 
	  then Ansi.print_colored 
	    [!norm_color, "\n  Scanning Modtimes: "; 
	     !path_color, path; 
	     !norm_color, "    "]
	  else printf "\n  Scanning Modtimes: %s     " path;
	  Pervasives.flush Pervasives.stdout;
	  let res = scan_modtimes ~print_progress:true (read_ldir path) in
	    print_newline ();
	    print_res "    " res;
	    res))
      paths in

  let total = List.fold_left plus zed counts in
    
(*    if !sort_output
    then 
      (let zipped = List.map2 pair counts paths in
       let sorted = List.sort (fun x y -> cmpr (fst x) (fst y)) zipped in
	 List.iter
	   (fun (cnt,path) -> 
	      if !use_color 
	      then Ansi.print_colored 
		[!norm_color, "\n  Summed "; 
		 !path_color, path ]
	      else printf "\n  Summed %s     " path;
	      print_newline ();
	      print_res "    " cnt)
	   sorted);*)
    
    if List.tl paths <> []
    then (printf "\nTotal: \n";
	  print_res "  " total);
    if !use_color
    then switch_color Ansi.Reset;;


main ();;
	   
