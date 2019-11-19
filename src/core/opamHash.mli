(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type kind = [ `MD5 | `SHA256 | `SHA512 ]
(** Stored as hexadecimal strings *)

type t

val kind : t -> kind

val contents : t -> string
(** The value of the hash, as a string of hexadecimal characters *)

val string_of_kind : kind -> string

val md5 : string -> t

val sha256 : string -> t

val sha512 : string -> t

include OpamStd.ABSTRACT with type t := t

val of_string_opt : string -> t option

val to_path : t -> string list
(** returns a sub-path specific to this hash, e.g.
    "md5/d4/d41d8cd98f00b204e9800998ecf8427e", as a list *)

val check_file : string -> t -> bool

val mismatch : string -> t -> t option
(** Like [check_file], but returns the actual mismatching hash of the file, or
    [None] in case of match *)

val compute : ?kind:kind -> string -> t
(** Compute hash of the given file *)

val compute_from_string : ?kind:kind -> string -> t
(** Compute the hash of the given string *)
