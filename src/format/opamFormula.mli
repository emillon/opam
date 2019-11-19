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

(** Formulas on packages, opt. with sub-formulas on versions, and conversion
    functions *)

type relop = OpamParserTypes.relop
(** binary operations (compatible with the Dose type for Cudf operators!) *)

(* = [ `Eq | `Neq | `Geq | `Gt | `Leq | `Lt ] *)

type version_constraint = relop * OpamPackage.Version.t
(** Version constraints for OPAM *)

type atom = OpamPackage.Name.t * version_constraint option
(** Formula atoms for OPAM *)

val string_of_atom : atom -> string
(** Pretty-printing of atoms *)

val short_string_of_atom : atom -> string
(** The compact atom format used in requests, "pkgOPvers", with '.' allowed instead
    of '=' *)

val string_of_atoms : atom list -> string
(** Prints atoms as a conjunction ("&") using the short format *)

val check : atom -> OpamPackage.t -> bool
(** Checks if a package verifies an atom *)

val packages_of_atoms :
  ?disj:bool -> OpamPackage.Set.t -> atom list -> OpamPackage.Set.t
(** Return all packages satisfying the given atoms from a set (i.e. name
    matching at least one of the atoms, version matching all atoms with the
    appropriate name). If [disj] is true, returns packages that satisfy at
    least one of the constraint of a given name, otherwise that satisfy all
    constraints. *)

type 'a conjunction = 'a list
(** AND formulas *)

val string_of_conjunction : ('a -> string) -> 'a conjunction -> string
(** Pretty print AND formulas *)

type 'a disjunction = 'a list
(** OR formulas *)

val string_of_disjunction : ('a -> string) -> 'a disjunction -> string
(** Pretty print OR formulas *)

type 'a cnf = 'a disjunction conjunction
(** CNF formulas (Conjunctive Normal Form) *)

type 'a dnf = 'a conjunction disjunction
(** DNF formulas (Disjunctive Normal Form) *)

val string_of_cnf : ('a -> string) -> 'a cnf -> string
(** Pretty print CNF formulas *)

val string_of_dnf : ('a -> string) -> 'a dnf -> string
(** Pretty print DNF formulas *)

(** General formulas *)
type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

val compare_formula : ('a -> 'a -> int) -> 'a formula -> 'a formula -> int

val eval : ('a -> bool) -> 'a formula -> bool
(** Eval a formula *)

val partial_eval :
  ('a -> [ `Formula of 'b formula | `True | `False ]) ->
  'a formula ->
  [ `Formula of 'b formula | `True | `False ]

val check_relop : relop -> int -> bool
(** Check a relational operator against an integer from compare *)

val eval_relop : relop -> OpamPackage.Version.t -> OpamPackage.Version.t -> bool
(** Evaluate a relational operator between versions *)

val neg_relop : relop -> relop

val string_of_formula : ('a -> string) -> 'a formula -> string
(** Pretty print a formula *)

val ands : 'a formula list -> 'a formula
(** Convert a list of formulas to an AND-formula ([Empty] formulas are
    ignored) *)

val ands_to_list : 'a formula -> 'a formula list
(** Converts back an AND-formula to a list (flattens top-level ands) *)

val ors : 'a formula list -> 'a formula
(** Convert a list of formulas to an OR-formula ([Empty] formulas are
    ignored) *)

val ors_to_list : 'a formula -> 'a formula list
(** Converts back an OR-formula to a list (flattens top-level ors) *)

val map : ('a -> 'b formula) -> 'a formula -> 'b formula
(** Map on atoms. Atoms for which the given function returns Empty
    will be simply removed *)

val map_formula : ('a formula -> 'a formula) -> 'a formula -> 'a formula
(** Maps top-down on a formula *)

val map_up_formula : ('a formula -> 'a formula) -> 'a formula -> 'a formula
(** Maps bottom-up on a formula (atoms first) *)

val neg : ('a -> 'a) -> 'a formula -> 'a formula
(** Negates a formula (given the function to negate atoms) *)

val iter : ('a -> unit) -> 'a formula -> unit
(** Iter function *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a
(** Fold function (bottom-up, left-to-right) *)

val fold_right : ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a
(** Fold function (bottom-up, right-to-left) *)

val sort : ('a -> 'a -> int) -> 'a formula -> 'a formula
(** Sort formula, using [compare] function. `Block` around `Or` and `And` \
    are removed. *)

type version_formula = version_constraint formula
(** Expressions composed entirely of version constraints *)

val check_version_formula : version_formula -> OpamPackage.Version.t -> bool
(** Checks if a given version satisfies a formula *)

type t = (OpamPackage.Name.t * version_formula) formula
(** An atom is: [name] * ([relop] * [version]) formula.
    Examples of valid formulae:
    - "foo" \{> "1" & (<"3" | ="5")\}
    - "foo" \{= "1" | > "4"\} | ("bar" "bouh") *)

val compare : t -> t -> int

val verifies : t -> OpamPackage.t -> bool
(** Returns [true] if [package] verifies [formula] (i.e. it is within at least
    one package set that is a solution of the formula, and is named in the
    formula) *)

val packages : OpamPackage.Set.t -> t -> OpamPackage.Set.t
(** Returns the subset of packages possibly matching the formula (i.e. including
    all disjunction cases) *)

val cnf_of_formula : 'a formula -> 'a formula
(** Convert a formula to CNF *)

val dnf_of_formula : 'a formula -> 'a formula
(** Convert a formula to DNF *)

val to_atom_formula : t -> atom formula
(** Transform a formula where versions can be expressed using formulas
    to a flat atom formula *)

val of_atom_formula : atom formula -> t
(** Convert an atom-formula to a t-formula *)

val simplify_ineq_formula :
  ('a -> 'a -> int) -> (relop * 'a) formula -> (relop * 'a) formula option
(** [simplify_ineq_formula comp f] returns a canonical version of inequality
    formula [f], based on comparison function [comp], where each version appears
    at most once, and in increasing order. Returns [Some Empty] if the formula
    is always [true], [None] if it is always false *)

val simplify_version_formula : version_formula -> version_formula option
(** Like [simplify_ineq_formula], but specialised on version formulas *)

val simplify_version_set :
  OpamPackage.Version.Set.t -> version_formula -> version_formula
(** A more aggressive version of [simplify_version_formula] that attempts to
    find a shorter formula describing the same subset of versions within a given
    set. The empty formula is returned for an empty set, and the original
    formula is otherwise returned as is if no versions match. *)

val formula_of_version_set :
  OpamPackage.Version.Set.t -> OpamPackage.Version.Set.t -> version_formula
(** [formula_of_version_set set subset] generates a formula that is enough to
    describe all packages of [subset] and exclude packages otherwise in [set] *)

(** {2 Atoms} *)

val atoms : t -> atom list
(** Return all the atoms *)

val to_string : t -> string
(** Pretty print the formula *)

val to_conjunction : t -> atom conjunction
(** Return a conjunction. If the initial formula is not a
    conjunction, then fail. *)

val of_conjunction : atom conjunction -> t
(** Return a formula from a conjunction of atoms *)

val to_disjunction : t -> atom disjunction
(** Return a disjunction of atoms from a package formula. It the initial formula
    is not a disjunction, then fail. *)

val set_to_disjunction : OpamPackage.Set.t -> t -> atom disjunction
(** Like [to_disjunction], but accepts conjunctions within constraint formulas,
    resolving them using the provided package set. Conjunctions between packages
    still raise [Failure]. *)

val of_disjunction : atom disjunction -> t
(** Return a formula from a disjunction of atoms *)

val to_cnf : t -> atom cnf
(** Return an equivalent CNF formula *)

val to_dnf : t -> atom dnf
(** Return an equivalent DNF formula *)
