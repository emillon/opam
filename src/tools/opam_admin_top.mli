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

(** Small lib for writing opam-repo admin scripts *)

val repo : OpamTypes.dirname
(** The current repo (taken from CWD!) *)

val packages : OpamPackage.Set.t
(** All defined packages in the current repo *)

open OpamFile

type 'a action = [ `Update of 'a | `Remove | `Keep ]

val iter_packages_gen :
  ?quiet:bool ->
  (OpamPackage.t ->
  prefix:string option ->
  opam:OPAM.t ->
  descr:Descr.t option ->
  url:URL.t option ->
  dot_install:Dot_install.t option ->
  OPAM.t * Descr.t action * URL.t action * Dot_install.t action) ->
  unit
(** Maps on the files of every package. Only changed files are written back to
    disk. *)

val filter_packages : string list -> OpamPackage.t -> bool
(** Turn a list of glob patterns into a proper filtering function on
    package names. *)

val iter_packages :
  ?quiet:bool ->
  ?filter:(OpamPackage.t -> bool) ->
  ?f:(OpamPackage.t -> string option -> OPAM.t -> unit) ->
  ?opam:(OpamPackage.t -> OPAM.t -> OPAM.t) ->
  ?descr:(OpamPackage.t -> Descr.t -> Descr.t) ->
  ?url:(OpamPackage.t -> URL.t -> URL.t) ->
  ?dot_install:(OpamPackage.t -> Dot_install.t -> Dot_install.t) ->
  unit ->
  unit
(** Quicker interface when considering a single type of file *)
