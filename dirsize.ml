(** dirsize.ml

  This program makes use of my little Dirtree library to build
  summaries of the contents of paths given as input on the command
  line.

  It also depends on my "Rutils" generic library for Ansi printing
  functions.

  --Ryan Newton
*)

open Printf
open Rutils
open Unix
open Dirtree

let version = "0.9"

let use_color   = ref false
let norm_color  = ref Ansi.Darkgray
let path_color  = ref Ansi.Red
let total_color = ref Ansi.Green
let count_color = ref Ansi.Lightgreen

let sort_output = ref false


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

(** Sorry I was messing with multiple vers, so this is wrapped up in a
  module: *)
module Newver = 
struct
  open Int64;;
  let zed =
    { files     = 0;
      filebytes = zero;
      dirs      = 0;
      dirbytes  = zero;
      links     = 0;
      linkbytes = zero }
  let plus a b =
    { files     = a.files + b.files;
      filebytes = add a.filebytes b.filebytes;
      dirs      = a.dirs + b.dirs;
      dirbytes  = add a.dirbytes b.dirbytes;
      links     = a.links + b.links;
      linkbytes = add a.linkbytes b.linkbytes }

  let cmpr a b = Int64.compare a.filebytes b.filebytes
    
  let count = Dirtree.dircount
  let print_res indent sum =
    if !use_color
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
end

(********************************************************************)


let print_version () = 
  print_string "ds - directory size: ver ";
  print_endline version;
  exit 0;;

let print_help () = 
  print_endline "Usage: dirsize/ds <options> <path-names>";
  print_endline "options are:";
  print_endline "  -h   Show this help message.";
  print_endline "  -v   Show the version.";
  print_endline "  -c   Use ANSI color.  Green and red theme.";
  print_endline "  -b   Use ANSI color.  Blue and yellow theme.";
  print_endline "  -nc  Don't use color.  Plain ASCII.";
  print_endline "  -s   Sort output by filesize, increasing.";
  exit 0;;

let pair a b = (a,b) 

(********************************************************************)
(* Main script *)

open Newver

let main () =

  let paths = (* Here are all those filepaths. *)
    (let temp = List.filter
		  (* Process flags *)
		  (function
		     | "-h"  -> print_help (); false
		     | "-v"  -> print_version (); false
		     | "-c"  -> use_color := true; false
		     | "-nc" -> use_color := false; false
		     | "-b"  -> use_color := true; 
			 path_color  := Ansi.Lightblue;
			 total_color := Ansi.Yellow;
			 false
		     | "-s"  -> sort_output := true; false
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
	 if not !sort_output 
	 then (if !use_color 
	       then Ansi.print_colored 
		 [!norm_color, "\n  Summing "; 
		  !path_color, path; 
		  !norm_color, "    "]
	       else printf "\n  Summing %s     " path;
	       Pervasives.flush Pervasives.stdout;
	       let res = count ~print_progress:true (read_ldir path) in
		 print_newline ();
		 print_res "    " res;
		 res)
	 else count ~print_progress:false (read_ldir path))
      paths in

  let total = List.fold_left plus zed counts in
    
    if !sort_output
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
	   sorted);
    
    if List.tl paths <> []
    then (printf "\nTotal: \n";
	  print_res "  " total);
    if !use_color
    then switch_color Ansi.Reset;;


main ();;
	   
