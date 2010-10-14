(** rutils.ml 
  
  These are generic utilities, some tree/list/array stuff mostly. 

  -Ryan Newton
*)


module RString :
  sig
    val string_of_charlist : char list -> string
    val charlist_of_string : string -> char list
(*    val string_reverse : string -> string*)
    val rev : string -> string
    val index_string : string -> string -> int
    val head : string -> int -> string
    val tail : string -> int -> string
    val map : (char -> 'a) -> string -> 'a list
  end

module RHashtbl :
  sig
    module HshOrd : sig type t = int val compare : 'a -> 'b -> int end
    module HshSet :
      sig
        type elt = HshOrd.t
      end
    val length : ('a, 'b) Hashtbl.t -> int
    val length_nodups : (HshSet.elt, 'a) Hashtbl.t -> int
    val get_random : ('a, 'b) Hashtbl.t -> ('a * 'b)
    val flatten : (HshSet.elt, 'a) Hashtbl.t -> (HshSet.elt, 'a) Hashtbl.t
    val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
    val to_list_nodups : (HshSet.elt, 'a) Hashtbl.t -> (HshSet.elt * 'a) list
    val map_bang : ('a -> 'b -> 'b) -> ('a, 'b) Hashtbl.t -> unit
  end


module RList :
  sig
    val init : int -> (int -> 'a) -> 'a list
    val biginit : int64 -> (int64 -> 'a) -> 'a list
    val make : int -> 'a -> 'a list
    val last : 'a list -> 'a
    val fissure : int -> 'a list -> 'a list * 'a list
    val prefix : int -> 'a list -> 'a list
    val postfix : int -> 'a list -> 'a list
    val chunk : int -> 'a list -> 'a list list
    val iteri : (int -> 'a -> 'b) -> 'a list -> unit
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

    val filter_some : 'a option list -> 'a list

    val find_some : ('a -> 'b option) -> 'a list -> 'b option 
      (** This ones like find, but instead of a predicate, it takes an
	option returning function and returns the return value. *)

    val index : 'a -> 'a list -> int
    val indexq : 'a -> 'a list -> int
      (** These find the index of (the first occurrence of) a given
	element in a given list.  They return -1 if the element is not
	present. *)

    val indices : 'a -> 'a list -> int list
    val index_all : ('a -> bool) -> 'a list -> int list
      (** This is like filter, but returns the indices of matching
	elements. *)

    val remove : 'a -> 'a list -> 'a list
      (** This version doesn't rebuild the tail of the list after the
	last removal. *)
    val remq : 'a -> 'a list -> 'a list
      (** This version doesn't rebuild the tail of the list after the
	last removal. *)

    val rem1q : 'a -> 'a list -> 'a list
      (** Removes the first instance of an item from a list, using
	physical equality.*)
    val removei : int -> 'a list -> ('a * 'a list)
      (** Removes an item with a given index from the list. Return the
	diminished list as well as the exact element removed. *)

    val replacei : int -> 'a -> 'a list -> 'a list
      (** This replaces the entry at a given index.  (Rebuilding the
	prefix of the list before that entry, of course.) *)      

    val replace : 'a -> 'a -> 'a list -> 'a list
      (** Replaces all the occurences of old with nu.  Use structural
	equality. It uses structural equality. *)
    val replace1 : 'a -> 'a -> 'a list -> 'a list
      (** Replaces only the first occurence of old with nu.  Raise
	Not_found if no occurence of old is found.  Use structural
	equality. *)
    val replaceq : 'a -> 'a -> 'a list -> 'a list
      (** Variant using physical equality *)
    val replace1q : 'a -> 'a -> 'a list -> 'a list
      (** Variant using physical equality *)


    val get_random : 'a list -> 'a
    val random_insert : 'a -> 'a list -> 'a list
    val random_remove : 'a list -> ('a * 'a list)
    val random_replace : 'a -> 'a list -> 'a list
    val randomize : 'a list -> 'a list

    val is_set : 'a list -> bool 
    val is_subset : 'a list -> 'a list -> bool 
      
    val to_set : ('a -> 'a -> int) -> 'a list -> 'a list
    val to_string : ('a -> string) -> 'a list -> string
    val to_string_nopunct : ('a -> string) -> 'a list -> string
    val print : ('a -> string) -> 'a list -> unit


    (** { Here are some heavier duty utilities that happen to operate
      over lists. } *)

    (** This is an abstract yet specific function which takes all
      "repeat" instances in a list and squishes them together,
      inserting them in the position of the first occurence.  It's
      parameterized by a comparison function and a compaction (squish)
      function. *)
    val compact : 'a list -> ('a -> 'a -> int) -> ('a -> 'a -> 'a) -> 'a list

    module Lon : 
    sig            
      (** Find_gap returns the value of the first gap in a sequence of
	increasing numbers.  Delta is the expected distance between each
	element of the list, usually 1 or -1.  The return value is an
	option type.  If the sequence of numbers are "consecutive" (there
	is no gap), then None is returned.

	Numbers may not fall between the gaps specified by the delta
	parameter (or an exception is raised).  But there may be repeated
	numbers.
      *)
      val find_gap : int -> int list -> int option 
	(*val find_gap : ('a -> 'a) -> 'a list -> 'a option *)

      (** Find_gaps returns a list of gaps. *)
      val find_gaps : int -> int list -> int list
    end
      
    module Sorted :
    sig
      val equality_partition : ('a -> 'a -> int) -> 'a list -> ('a list list)
      val separate_dups : ('a -> 'a -> int) -> 'a list -> ('a list) * ('a list)
      val insert : ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

      val is_set :    ('a -> 'a -> int) -> 'a list -> bool 
      val is_subset : ('a -> 'a -> int) -> 'a list -> 'a list -> bool 
    end
      
    val separate_dups        : ('a -> 'a -> int) -> 'a list -> ('a list) * ('a list)
    val random_get : 'a list -> 'a
    val shuffle : 'a list -> 'a list

    end

module RArray :
sig
  val rotate : 'a array array -> 'a array array
  val randomize : 'a array -> unit
  val get_random : 'a array -> 'a

  val memq   : 'a -> 'a array -> bool

  val index  : 'a -> 'a array -> int
  val indexq : 'a -> 'a array -> int
    (** These find the index of (the first occurrence of) a given
      element in a given array.  They return -1 if the element is not
      present. *)
    
  val to_string : ('a -> string) -> 'a array -> string
  val print : ('a -> string) -> 'a array -> unit

  module Sorted :
  sig
    val binsearch : ('a -> 'a -> int) -> 'a -> 'a array -> int option
    val nearest   : ('a -> 'a -> int) -> 'a -> 'a array -> int
      (** When using this function on Arrays of numbers, it is
	important to remember that this returns an *INDEX* to the
	nearest, not the nearest itself. *)
  end

  module Aon :
  sig
    val nearest : int -> int array -> int
      (** Raises (Failure "nearest: empty array") if the array is
	empty. *)
      (** It is important to remember that this returns an *INDEX* to
	the nearest, not the nearest itself. *)
  end
end

module Graphviz :
sig
  val dotty_graphs : ('a -> 'a list) -> ('a -> string) -> 'a array -> unit
end

module Ansi : 
sig 
  type color = 
      Reset  
    | Bold_on  | Italics_on  | Underline_on  | Inverse_on 
    | Bold_off | Italics_off | Underline_off | Inverse_off 
    | Black  | Lightgray | Darkgray 
    | Blue   | Lightblue 
    | Green  | Lightgreen
    | Cyan   | Lightcyan 
    | Red    | Lightred 
    | Purple | Lightpurple
    | Brown  | Yellow    | White
  val color_code :  color -> string

  val is_color_str : string -> bool
  val color_of_string : string -> color	  
  val print_colored_strs : string list -> unit
    (** This is a deprecated version that allows you to just give a
      list of strings with color-names interspersed.  *)

  val switch_color : color -> unit
    (** Switch_color echoes an ansi sequence which should switch your
      terminal into the given color. *)

  val print_colored : (color * string) list -> unit
    (** Print_colored prints each string in the color specified. *)

  val test_colors : unit -> unit
    (** Test_colors does a little color test that you can use to make
      sure your terminal supports ansi properly *)
end 

module Misc :
sig
  val id : 'a -> 'a
  val applyn : int -> ('a -> 'a) -> ('a -> 'a)
  val ( @@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
  val ( @< ) : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b
  val ( += ) : int ref -> int -> unit
  val ( +=. ) : float ref -> float -> unit

  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

  val stream_random : unit -> 'a -> 'a

  (*  val random_tree : 
      make_branch:(int -> (int -> 'a list) -> 'a) -> 
      depth:int  -> 'a*)
  val random_tree : (int -> (int -> 'a list) -> 'a) -> int  -> 'a
    (** random_tree : make_branch -> max_depth -> tree *)
    (** This makes random trees with a simple method.  The make_branch
      funciton is a little confusing.  It is the constructor function
      that random_tree uses to actually build the nodes.
      Its first argument is an integer message (arity).  Its second
      argument is an children-making function (mapping integers onto
      lists of children). If -1 is provided as arity, make_branch is
      expected to make a random functional (non-terminal) node.  (That
      is make_branch should not call the children-building function if
      -1 is provided as arity.). *)
  val random_complete_tree : (int -> (int -> 'a list) -> 'a) -> int  -> 'a
    (**  random_tree : make_branch -> depth -> tree  *)
    (** This variation makes complete trees (i.e. all leaves at the
      same depth.) *)

  val random_int64 : unit -> int64

  (** {6 Some option type shorthands... } *)
  val unSome : 'a -> 'a option -> 'a 
    (** Pops the option type, using a provided default value in place of None.*)
  val unoption : 'a -> 'a option -> 'a 
    (** An alias for unSome. *)
  val is_none : 'a option -> bool
  val is_some : 'a option -> bool
  val list_of_option : 'a option -> 'a list

  val string_of_loi : int list -> string
end
