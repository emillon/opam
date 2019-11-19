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

(** Opam CLI main entry point *)

open Cmdliner

(** {2 Commands} *)

type command = unit Term.t * Term.info
(** Type of commands *)

val commands : command list
(** The default list of commands *)

val default : command
(** opam *)

val init : command
(** opam init *)

val list : ?force_search:bool -> unit -> command
(** opam list *)

val show : command
(** opam show *)

val install : command
(** opam install *)

val remove : command
(** opam remove *)

val reinstall : command
(** opam reinstall *)

val update : command
(** opam update *)

val upgrade : command
(** opam upgrade *)

val config : command
(** opam config *)

val repository : command
(** opam repository *)

val switch : command
(** opam switch *)

val pin : ?unpin_only:bool -> unit -> command
(** opam pin *)

val help : command
(** opam help *)
