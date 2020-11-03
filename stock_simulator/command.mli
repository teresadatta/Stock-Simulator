(** Parsing of user input commands within the simulator.*)

(** Type [obj] represents either a string or an int. *)
type obj = 
  | String of string
  | Int of int

(** The type [object_phrase] represents the object phrase that is a 
      user command. Each element of the list is a word: sequence of non-space 
      characters. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Port
  | Buy of object_phrase 
  | Sell of object_phrase 
  | End
  | Next
  | Board
  | Info
  | Leader
  | Cash
  | Liquidate

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [listify str] changes a string into a list of words (i.e., consecutive 
    sequence of non-space characters)*)
val listify : string -> string list 

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is none of the commands.*)
val parse : string -> command
