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

(** Functions handling the "opam config" subcommand *)

open OpamTypes
open OpamStateTypes

val env :
  'a global_state ->
  switch ->
  ?set_opamroot:bool ->
  ?set_opamswitch:bool ->
  csh:bool ->
  sexp:bool ->
  fish:bool ->
  inplace_path:bool ->
  unit
(** Display the current environment. Booleans csh, sexp and fish set an alternative
    output (unspecified if more than one is true, sh-style by default).
    [inplace_path] changes how the PATH variable is updated when there is already
    an opam entry: either at the same rank, or pushed in front. *)

val ensure_env : 'a global_state -> switch -> unit
(** Ensures that the environment file exists in the given switch, regenerating
    it, if necessary. *)

val print_eval_env : csh:bool -> sexp:bool -> fish:bool -> env -> unit
(** Like [env] but allows one to specify the precise env to print rather than
    compute it from a switch state *)

val list : 'a global_state -> name list -> unit
(** Display the content of all available variables; global summary if the list
    is empty, package name "-" is understood as global configuration *)

val variable : 'a global_state -> full_variable -> unit
(** Display the content of a given variable *)

val subst : 'a global_state -> basename list -> unit
(** Substitute files *)

val expand : 'a global_state -> string -> unit
(** Prints expansion of variables in string *)

val set : full_variable -> string option -> unit
(** Sets or unsets switch config variables *)

val set_global : full_variable -> string option -> unit
(** Sets or unsets global config variables *)

val exec :
  [< unlocked ] global_state ->
  ?set_opamroot:bool ->
  ?set_opamswitch:bool ->
  inplace_path:bool ->
  string list ->
  unit
(** Execute a command in a subshell, after variable expansion *)
