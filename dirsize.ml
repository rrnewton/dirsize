open Printf
open Utils
open Unix
open Dirtree

let comma_str s = 
  let ls = RString.charlist_of_string s in
  let sep = RList.chunk 3 (List.rev ls) in
  let flipped = List.rev (List.map List.rev sep) in
  let strs = List.map RString.string_of_charlist flipped in    
    String.concat "," strs
      
let comma_int i = comma_str (string_of_int i)
let comma_int64 i = comma_str (Int64.to_string i)

(*  let strip_slash s =
    let len = String.length s in
      if len=0 || s.[len-1] <> '/'
      then s
      else String.sub s 0 (len-1)*)


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
    printf "%s%10s bytes in %d plain files.\n" 
      indent (comma_int64 sum.filebytes) sum.files;
    printf "%s%10s bytes in %d directories.\n" 
      indent (comma_int64 sum.dirbytes) sum.dirs;
    printf "%s%10s bytes in %d symlink files.\n" 
      indent (comma_int64 sum.linkbytes) sum.links;
    printf "%s%10s bytes total.\n" 
      indent (comma_int64
		(add (add sum.filebytes sum.dirbytes) sum.linkbytes ));
end

(* Main script *)

let main (count : lfiletree->dircount_t) 
  (printres : string -> dircount_t -> unit)
  plus zed =

  let paths = 
    if Array.length Sys.argv < 2 
    then [Filename.current_dir_name]
    else List.tl (Array.to_list Sys.argv) in
  let sum = 
    List.fold_left 
      (fun sum path ->
	 printf "\n  Summing %s     " path;
	 Pervasives.flush Pervasives.stdout;
	 let res = count (read_ldir path) in
	   print_newline ();
	   printres "    " res;
	   plus res sum)
      zed paths 
  in 
    if List.tl paths <> []
    then (printf "\nTotal: \n";
	  printres "  " sum);;


main 
  Newver.dircount
  Newver.print_res 
  Newver.plus
  Newver.zed;;



(*main 
  Oldver.dircount
  Oldver.print_res 
  Oldver.plus
  Oldver.zed;;*)

	   
