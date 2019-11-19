(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Console output, ANSI color, logging and user querying *)

(** Global configuration parameters (read from OpamGlobalConfig, and the
    environment when necessary) *)

val debug : unit -> bool

val verbose : unit -> bool

val color : unit -> bool

val utf8 : unit -> bool

val utf8_extended : unit -> bool

val disp_status_line : unit -> bool

(** General text formatting *)

type text_style =
  [ `black
  | `blue
  | `bold
  | `crossed
  | `cyan
  | `green
  | `magenta
  | `red
  | `underline
  | `white
  | `yellow ]
(** Settable attributes for ANSI terminal output. Nesting is generally not
    handled. *)

val colorise : text_style -> string -> string
(** Helper coloring functions. Returns the string unchanged if color is
    disabled *)

val colorise' : text_style list -> string -> string

val acolor : text_style -> unit -> string -> string

val acolor_w : int -> text_style -> out_channel -> string -> unit

module Symbols : sig
  val rightwards_arrow : OpamCompat.Uchar.t

  val box_drawings_light_down_and_right : OpamCompat.Uchar.t

  val box_drawings_light_horizontal : OpamCompat.Uchar.t

  val box_drawings_light_vertical : OpamCompat.Uchar.t

  val box_drawings_light_up_and_right : OpamCompat.Uchar.t

  val box_drawings_light_right : OpamCompat.Uchar.t

  val circled_division_slash : OpamCompat.Uchar.t

  val asterisk_operator : OpamCompat.Uchar.t

  val north_east_arrow : OpamCompat.Uchar.t

  val south_east_arrow : OpamCompat.Uchar.t

  val clockwise_open_circle_arrow : OpamCompat.Uchar.t

  val greek_small_letter_lambda : OpamCompat.Uchar.t

  val latin_capital_letter_o_with_stroke : OpamCompat.Uchar.t

  val six_pointed_black_star : OpamCompat.Uchar.t

  val upwards_arrow : OpamCompat.Uchar.t

  val downwards_arrow : OpamCompat.Uchar.t

  val up_down_arrow : OpamCompat.Uchar.t

  val downwards_double_arrow : OpamCompat.Uchar.t

  val downwards_black_arrow : OpamCompat.Uchar.t

  val black_down_pointing_triangle : OpamCompat.Uchar.t
end

val utf8_symbol :
  OpamCompat.Uchar.t -> ?alternates:OpamCompat.Uchar.t list -> string -> string

(** Logging *)

val timer : unit -> unit -> float
(** Timers, only active when debug is on. Returns the time between the
    application to each argument, in seconds *)

val log : string -> ?level:int -> ('a, out_channel, unit) format -> 'a
(** [log section ~level fmt args]. Used for debug messages, default
    level is 1 *)

val slog : ('a -> string) -> out_channel -> 'a -> unit
(** Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
    rather than [log "%s" (to_string x)] to avoid costly unneeded
    stringifications *)

val error : ('a, unit, string, unit) format4 -> 'a

val warning : ('a, unit, string, unit) format4 -> 'a

val note : ('a, unit, string, unit) format4 -> 'a

val errmsg : ('a, unit, string, unit) format4 -> 'a
(** Message without prefix, reformat or newline, to stderr (useful to continue
    error messages without repeating "[ERROR]") *)

val error_and_exit :
  OpamStd.Sys.exit_reason -> ('a, unit, string, 'b) format4 -> 'a

val msg : ('a, unit, string, unit) format4 -> 'a

val formatted_msg : ?indent:int -> ('a, unit, string, unit) format4 -> 'a

val header_msg : ('a, unit, string, unit) format4 -> 'a

val header_error :
  ('a, unit, string, ('b, unit, string, unit) format4 -> 'b) format4 -> 'a

val carriage_delete : unit -> unit
(** Erase the current line on stdout (doesn't flush stdout) *)

val status_line : ('a, unit, string, unit) format4 -> 'a
(** Display a dynamic status line to stdout, that will be erased on next call.
    The message should not be wider than screen nor contain newlines. Use
    {!clear_status} when the status line should be erased. *)

val clear_status : unit -> unit
(** Erase the status line and restore the cursor to the start of the line *)

val confirm : ?default:bool -> ('a, unit, string, bool) format4 -> 'a
(** Ask the user to press Y/y/N/n to continue (returns a boolean).
    Defaults to true (yes) if unspecified *)

val read : ('a, unit, string, string option) format4 -> 'a
(** Read some input from the user (returns a string option) *)

val print_table :
  ?cut:[ `Wrap of string | `Truncate | `None ] ->
  out_channel ->
  sep:string ->
  string list list ->
  unit
(** Prints a table; generally called on tables passed through [align_table].
    The default [cut] is to wrap on stdout, stderr, keep as-is otherwise.
    [`Wrap sep] prepends [sep] on wrapped lines *)
