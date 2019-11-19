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

(** (generated) Current OPAM version *)

include OpamStd.ABSTRACT

val current : t
(** The current OPAM version *)

val major : t -> t
(** Extracts the major version *)

val nopatch : t -> t
(** Major+minor version, strips the patch version *)

val current_nopatch : t
(** The current OPAM version, truncated (only MAJOR.MINOR) *)

val git : unit -> t option
(** The 'git' version of OPAM *)

val set_git : string -> unit
(** Side-effect to set the git version later in the build *)

val full : unit -> t
(** The full version (current + git) *)

val magic : unit -> string
(** Magic string, always of length 8 *)

val message : unit -> unit
(** Display the version message *)

val compare : t -> t -> int
(** Version comparison *)
