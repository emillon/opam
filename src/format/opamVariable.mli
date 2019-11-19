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

(** OPAM variables with scope (global or module), used in "opam" package
    definition files in "filters" *)

(** {2 Variable names} *)

include OpamStd.ABSTRACT

type variable = t
(** Shortcut to variables *)

(** Variable contents *)
type variable_contents = B of bool | S of string | L of string list

val string_of_variable_contents : variable_contents -> string
(** Pretty print of variable contents *)

(** Variable contents constructors *)

val string : string -> variable_contents

val int : int -> variable_contents

val bool : bool -> variable_contents

val dirname : OpamFilename.Dir.t -> variable_contents

module Full : sig
  (** Fully qualified variable. *)

  include OpamStd.ABSTRACT

  type scope =
    | Global
        (** Note: this is attributed to unqualified variables, and may
                 also design self-referring ones *)
    | Self
        (** Variable in a package-specific file referring to that
               package [_:varname] *)
    | Package of OpamPackage.Name.t  (** [pkgname:varname] *)

  val scope : t -> scope
  (** Returns the scope of the variable *)

  val variable : t -> variable
  (** Returns the unqualified variable name *)

  val is_global : t -> bool

  val package : ?self:OpamPackage.Name.t -> t -> OpamPackage.Name.t option
  (** Return the package corresponding to the scope of the variable *)

  val create : OpamPackage.Name.t -> variable -> t
  (** Create a variable local for a given library/syntax extension *)

  val global : variable -> t
  (** Create a global variable *)

  val self : variable -> t
  (** Create a variable in the [Self] scope *)

  val read_from_env : t -> variable_contents option
  (** Looks up for an environment override through the environment, by means of
      [OPAMVAR_glovar] or [OPAMVAR_pkg_pkgvar] *)
end
