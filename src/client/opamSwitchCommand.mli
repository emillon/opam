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

(** Functions handling the "opam switch" subcommand *)

open OpamTypes
open OpamStateTypes

val install :
  rw global_state ->
  rt:'a repos_state ->
  ?synopsis:string ->
  ?repos:repository_name list ->
  update_config:bool ->
  packages:atom conjunction ->
  ?local_compiler:bool ->
  switch ->
  unlocked global_state * rw switch_state
(** Install a new switch, with the given packages set as compiler. The given
    [global_state] is unlocked as soon as possible, i.e. after registering the
    existence of the new switch. [update_config] sets the switch as current
    globally, unless it is external *)

val install_compiler_packages :
  rw switch_state -> atom conjunction -> rw switch_state
(** Install a compiler's base packages *)

val import :
  rw switch_state ->
  OpamFile.SwitchExport.t OpamFile.t option ->
  rw switch_state
(** Import a file which contains the packages to install.  *)

val export : ?full:bool -> OpamFile.SwitchExport.t OpamFile.t option -> unit
(** Export a file which contains the installed packages. If full is specified
    and true, export metadata of all installed packages (excluding overlay
    files) as part of the export. [None] means export to stdout. *)

val remove : rw global_state -> ?confirm:bool -> switch -> rw global_state
(** Remove the given compiler switch, and returns the updated state (unchanged
    in case [confirm] is [true] and the user didn't confirm) *)

val switch : 'a lock -> rw global_state -> switch -> unit
(** Changes the currently active switch *)

val reinstall : rw switch_state -> rw switch_state
(** Reinstall the given compiler switch. *)

val set_compiler :
  rw switch_state -> (name * version option) list -> rw switch_state
(** Sets the packages configured as the current switch compiler base *)

val show : unit -> unit
(** Display the current compiler switch. *)

val list : 'a global_state -> print_short:bool -> unit
(** List all the available compiler switches. *)

val get_compiler_packages :
  ?repos:repository_name list -> 'a repos_state -> package_set
(** Returns all available compiler packages from a repo state *)

val guess_compiler_package :
  ?repos:repository_name list -> 'a repos_state -> string -> atom option
(** Guess the compiler from the switch name: within compiler packages,
    match [name] against "pkg.version", "pkg", and, as a last resort,
    "version" (for compat with older opams, eg. 'opam switch 4.02.3') *)
