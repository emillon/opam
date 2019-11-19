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

(** Entry point to the solver, conversion of opam package universes to Cudf,
    dependencies computation. Front-end to Dose. *)

open OpamTypes

module Action : OpamActionGraph.ACTION with type package = package

module ActionGraph : OpamActionGraph.SIG with type package = package

type solution

val empty_universe : universe

(** {2 Solver} *)

val string_of_request : atom request -> string
(** Convert a request to a string *)

val stats : solution -> stats
(** Compute statistics about a solution *)

val new_packages : solution -> package_set
(** Return the new packages in the solution *)

val all_packages : solution -> package_set
(** Return all packages appearing in the solution *)

val string_of_stats : stats -> string
(** Pretty-printing of statistics *)

val solution_is_empty : solution -> bool
(** Is the solution empty? *)

val print_solution :
  messages:(package -> string list) ->
  append:(package -> string) ->
  requested:name_set ->
  reinstall:package_set ->
  solution ->
  unit
(** Display a solution *)

val solution_to_json : solution OpamJson.encoder
(** Serialize a solution *)

val solution_of_json : solution OpamJson.decoder

val cudf_versions_map : universe -> package_set -> int OpamPackage.Map.t
(** Computes an opam->cudf version map from a set of package *)

val load_cudf_universe :
  universe ->
  ?version_map:int package_map ->
  package_set ->
  ?depopts:bool ->
  build:bool ->
  post:bool ->
  unit ->
  Cudf.universe
(** Creates a CUDF universe from an OPAM universe, including the given packages.
    Evaluation of the first 3 arguments is staged. Warning: when [depopts] is
    [true], the optional dependencies may become strong dependencies. *)

val request :
  ?criteria:solver_criteria ->
  ?install:atom list ->
  ?upgrade:atom list ->
  ?remove:atom list ->
  unit ->
  atom request
(**  Build a request *)

val resolve :
  universe ->
  orphans:package_set ->
  atom request ->
  (solution, OpamCudf.conflict) result
(** Given a description of packages, return a solution preserving the
    consistency of the initial description. *)

val get_atomic_action_graph : solution -> ActionGraph.t
(** Returns the graph of atomic actions (rm, inst) from a solution *)

val installable : universe -> package_set
(** Keep only the packages that are installable. *)

val installable_subset : universe -> package_set -> package_set
(** Like [installable], but within a subset and potentially much faster *)

val dependencies :
  depopts:bool ->
  build:bool ->
  post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe ->
  package_set ->
  package list
(** Return the topological sort of the transitive dependency closures
    of a collection of packages.*)

val reverse_dependencies :
  depopts:bool ->
  build:bool ->
  post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe ->
  package_set ->
  package list
(** Same as [dependencies] but for reverse dependencies *)

module PkgGraph : Graph.Sig.I with type V.t = OpamPackage.t

val dependency_graph :
  depopts:bool ->
  build:bool ->
  post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe ->
  PkgGraph.t

val check_for_conflicts : universe -> OpamCudf.conflict option
(** Check the current set of installed packages in a universe for
    inconsistencies *)

val coinstallability_check : universe -> package_set -> OpamCudf.conflict option
(** Checks the given package set for complete installability ; returns None if
    they can all be installed together *)

val atom_coinstallability_check : universe -> atom list -> bool
(** Checks if the given atoms can be honored at the same time in the given
    universe *)

val dump_universe : universe -> out_channel -> unit
(** Dumps a cudf file containing all available packages in the given universe,
    plus version bindings (as '#v2v' comments) for the other ones. *)

val filter_solution : (package -> bool) -> solution -> solution
(** Filters actions in a solution. Dependents of a removed actions are removed
    to keep consistency *)
