open Printf
open Rutils
open Unix
open Dirtree

let use_color   = ref false
let norm_color  = ref Ansi.Darkgray
let path_color  = ref Ansi.Red
let total_color = ref Ansi.Green
let count_color = ref Ansi.Lightgreen


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

(* This was my old definition *)
(*let rec dirsize_old () = 
  let files,dirs,links = expand_dir "." in
  let s,f,d,l = 
    List.fold_left 
      (fun (s,f,d,l) (dir,_) -> 
	 Printf.printf "pushing %s\n" dir;
	 chdir dir; 
	 let (s',f',d',l') = dirsize_old () in 
	   Printf.printf "  popping...\n";
	   chdir ".."; 
	   (s+s',f+f',d+d',l+l'))
      (0,0,0,0) dirs in    
  let thissize = 
    List.fold_left (fun s (_,stat) -> s + stat.st_size) 0 files 
  in
    (thissize + s,
     List.length files + f,
     List.length dirs  + d,
     List.length links + l);; 

module Oldver =
struct
  let zed = ((0,0), (0,0), (0,0))
  let plus
    ((f,fs), (d,ds), (l,ls)) 
    ((f',fs'), (d',ds'), (l',ls')) =
    ((f+f',fs+fs'), (d+d',ds+ds'), (l+l',ls+ls'))
  let dircount = dircount
  let print_res indent ((f,fs), (d,ds), (l,ls)) =  
    printf "%s%11s bytes in %d plain files.\n" indent (comma_int fs) f;
    printf "%s%11s bytes in %d directories.\n" indent (comma_int ds) d;
    printf "%s%11s bytes in %d symlink files.\n" indent (comma_int ls) l;
    printf "%s%11s bytes total.\n" indent (comma_int (fs+ds+ls))
end  *)


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
  let dircount = Dirtree.dircount
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

(* Main script *)

let main (count : lfiletree->dircount_t) 
  (printres : string -> dircount_t -> unit)
  plus zed =

  let paths = 
    (let temp = List.filter
		  (function
		       "-c"  -> use_color := true; false
		     | "-nc" -> use_color := false; false
		     | "-b"  -> use_color := true; 
			 path_color  := Ansi.Lightblue;
			 total_color := Ansi.Yellow;
			 false
		     | _     -> true)
		  (List.tl (Array.to_list Sys.argv)) in
    if temp = []
      (* DEBUGGING: *) 
    (*then ["/ffh/ryan/home"]*)
    then [Filename.current_dir_name]
    else temp)
  in
  let sum = 
    List.fold_left 
      (fun sum path ->
	 if !use_color 
	 then Ansi.print_colored 
	   [!norm_color, "\n  Summing "; 
	    !path_color, path; 
	    !norm_color, "    "]
	 else printf "\n  Summing %s     " path;
	 Pervasives.flush Pervasives.stdout;
	 let res = count (read_ldir path) in
	   print_newline ();
	   printres "    " res;
	   plus res sum)
      zed paths 
  in 
    if List.tl paths <> []
    then (printf "\nTotal: \n";
	  printres "  " sum);
    if !use_color
    then switch_color Ansi.Reset;;

main 
  Newver.dircount
  Newver.print_res 
  Newver.plus
  Newver.zed;;

(*Ansi.test_colors ();;*)

(*main 
  Oldver.dircount
  Oldver.print_res 
  Oldver.plus
  Oldver.zed;;*)

	   
