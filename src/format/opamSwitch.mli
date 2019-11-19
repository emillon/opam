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

(** The type for switch names *)

include OpamStd.ABSTRACT

val unset : t
(** System switch name *)

val is_external : t -> bool
(** Determines wether this switch is internal (bound to a prefix within the opam
    root) or living somewhere else, in which case its prefix dir is inferred
    from its name using [get_root] *)

val get_root : OpamFilename.Dir.t -> t -> OpamFilename.Dir.t
(** Returns the root directory of the switch with the given name, assuming the
    given opam root *)

val external_dirname : string
(** The relative dirname in which the opam switch prefix sits for external
    switches ("_opam") *)

val of_dirname : OpamFilename.Dir.t -> t
(** Returns an external switch handle from a directory name. Resolves to the
    destination if [external_dirname] at the given dir is a symlink to another
    [external_dirname]. *)
