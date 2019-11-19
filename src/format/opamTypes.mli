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

(** Definitions of many types used throughout *)

type 'a success = [ `Successful of 'a ]
(** {2 Error and continuation handling} *)

type 'a error = [ `Error of 'a | `Exception of exn ]

type ('a, 'b) status = [ 'a success | 'b error ]

(** {2 Untyped generic file format} *)

include module type of struct
  include OpamParserTypes
end

(** {2 Filenames} *)

type basename = OpamFilename.Base.t
(** Basenames *)

type dirname = OpamFilename.Dir.t
(** Directory names (translated to absolute) *)

type filename = OpamFilename.t
(** Filenames *)

type filename_set = OpamFilename.Set.t
(** Set of files *)

type 'a filename_map = 'a OpamFilename.Map.t
(** Map of files *)

(** Predefined installation directories within a switch *)
type std_path =
  | Prefix
  | Lib
  | Bin
  | Sbin
  | Share
  | Doc
  | Etc
  | Man
  | Toplevel
  | Stublibs

(** Download result *)
type 'a download =
  | Up_to_date of 'a
  | Not_available of string option * string
      (** Arguments are respectively the short and long version of an error message.
      The usage is: the first argument is displayed on normal mode (nothing
      if [None]), and the second one on verbose mode. *)
  | Result of 'a

(** {2 Packages} *)

type package = OpamPackage.t = private {
  name : OpamPackage.Name.t;
  version : OpamPackage.Version.t;
}
(** Packages are ([name] * [version]) tuple *)

type package_set = OpamPackage.Set.t
(** Set of packages *)

type 'a package_map = 'a OpamPackage.Map.t
(** Map of packages *)

type name = OpamPackage.Name.t
(** Package names *)

type name_set = OpamPackage.Name.Set.t
(** Set of package names *)

type 'a name_map = 'a OpamPackage.Name.Map.t
(** Map of package names *)

type version = OpamPackage.Version.t
(** Package versions *)

type version_set = OpamPackage.Version.Set.t
(** Set of package versions *)

type opam_version = OpamVersion.t
(** OPAM versions *)

(** {2 Variables} *)

type variable = OpamVariable.t
(** Variables *)

type full_variable = OpamVariable.Full.t
(** Fully qualified variables (ie. with the name of
    sections/sub-sections they appear in) *)

(** Content of user-defined variables *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string
  | L of string list

type variable_map = OpamVariable.variable_contents OpamVariable.Map.t
(** A map from variables to their contents (i.e an environment) *)

(** Opam package flags *)
type package_flag =
  | Pkgflag_LightUninstall
      (** The package doesn't require downloading to uninstall *)
  | Pkgflag_Verbose
      (** The package's scripts output is to be displayed to the user *)
  | Pkgflag_Plugin
      (** The package is an opam plugin that will install a
                       [opam-<name>] exec, and may be auto-installed when doing
                       [opam <name>] *)
  | Pkgflag_Compiler  (** Package may be used for 'opam switch' *)
  | Pkgflag_Conf
      (** Virtual package: no source, no install or remove instructions,
                     .install, but likely has depexts *)
  | Pkgflag_Unknown of string
      (** Used for error reporting, otherwise ignored *)

(** At some point we want to abstract so that the same functions can be used
    over CUDF and OPAM packages *)
module type GenericPackage = sig
  include OpamParallel.VERTEX

  val name_to_string : t -> string

  val version_to_string : t -> string
end

(** {2 Formulas} *)

(** A generic formula *)
type 'a generic_formula = 'a OpamFormula.formula =
  | Empty
  | Atom of 'a
  | Block of 'a generic_formula
  | And of 'a generic_formula * 'a generic_formula
  | Or of 'a generic_formula * 'a generic_formula

type atom = OpamFormula.atom
(** Formula atoms *)

type formula = OpamFormula.t
(** Formula over versionned packages *)

type 'a conjunction = 'a OpamFormula.conjunction
(** AND formulat *)

type 'a disjunction = 'a OpamFormula.disjunction
(** OR formulat *)

(** {2 Repositories} *)

type repository_name = OpamRepositoryName.t
(** Repository names *)

type 'a repository_name_map = 'a OpamRepositoryName.Map.t
(** Maps of repository names *)

type url = OpamUrl.t

(*= {
  transport: string;
  path: string;
  hash: string option;
  backend: OpamUrl.backend;
} *)

type trust_anchors = { quorum : int; fingerprints : string list }

type repository = {
  repo_name : repository_name;
  repo_url : url;
  repo_trust : trust_anchors option;
}
(** Repositories *)

(** {2 Variable-based filters} *)

type filter =
  | FBool of bool
  | FString of string
  | FIdent of (name option list * variable * (string * string) option)
      (** packages (or None for self-ref through "_"), variable name,
      string converter (val_if_true, val_if_false_or_undef) *)
  | FOp of filter * relop * filter
  | FAnd of filter * filter
  | FOr of filter * filter
  | FNot of filter
  | FDefined of filter
  | FUndef of filter
      (** Returned by reduce functions when the filter could not be resolved to an
      atom (due to undefined variables or string expansions). The argument
      contains the partially reduced filter, where strings may still contain
      expansions (and are otherwise escaped). Used both for partial evaluation,
      and error messaging. Not allowed as an argument to other filters *)

(** {2 Filtered formulas (to express conditional dependencies)}

    These are first reduced to only the dependency-flag variables build, doc,
    dev, test defined in [Opam formulas] *)

type 'a filter_or_constraint = Filter of filter | Constraint of (relop * 'a)

type filtered_formula =
  (name * filter filter_or_constraint OpamFormula.formula) OpamFormula.formula

(** {2 Solver} *)

type 'a atomic_action = [ `Remove of 'a | `Install of 'a ]
(** Used internally when computing sequences of actions *)

type 'a highlevel_action =
  [ 'a atomic_action | `Change of [ `Up | `Down ] * 'a * 'a | `Reinstall of 'a ]
(** Used to compact the atomic actions and display to the user in a more
    meaningful way *)

type 'a inst_action = [ `Install of 'a | `Change of [ `Up | `Down ] * 'a * 'a ]
(** Sub-type of [highlevel_action] corresponding to an installed package that
    changed state or version *)

type 'a concrete_action = [ 'a atomic_action | `Build of 'a | `Fetch of 'a ]
(** Used when applying solutions, separates build from install *)

type 'a action = [ 'a atomic_action | 'a highlevel_action | 'a concrete_action ]

(** The possible causes of an action. *)
type 'a cause =
  | Use of 'a list
  | Required_by of 'a list
  | Conflicts_with of 'a list
  | Upstream_changes
  | Requested
  | Unknown

type actions_result = {
  actions_successes : package action list;
  actions_errors : (package action * exn) list;
  actions_aborted : package action list;
}
(** Solver result *)

type solution_result =
  | Nothing_to_do
  | OK of package action list  (** List of successful actions *)
  | Aborted
  | Partial_error of actions_result

(** Solver result *)
type ('a, 'b) result = Success of 'a | Conflicts of 'b

type solver_criteria = [ `Default | `Upgrade | `Fixup ]

type 'a request = {
  criteria : solver_criteria;
  wish_install : 'a conjunction;
  wish_remove : 'a conjunction;
  wish_upgrade : 'a conjunction;
  extra_attributes : string list;
}
(** Solver request *)

(** user request action *)
type user_action =
  | Query
  | Install
  | Upgrade
  | Reinstall
  | Remove
  | Switch
  | Import

type universe = {
  u_packages : package_set;
  u_installed : package_set;
  u_available : package_set;
  u_depends : filtered_formula package_map;
  u_depopts : filtered_formula package_map;
  u_conflicts : formula package_map;
  u_action : user_action;
  u_installed_roots : package_set;
  u_pinned : package_set;
  u_base : package_set;
  u_reinstall : package_set;
  u_attrs : (string * package_set) list;
}
(** Solver universe *)

(** {2 Command line arguments} *)

type pin_kind = [ `version | OpamUrl.backend ]
(** Pin kind *)

(** Shell compatibility modes *)
type shell = OpamStd.Sys.shell = SH_sh | SH_bash | SH_zsh | SH_csh | SH_fish

(** {2 Generic command-line definitions with filters} *)

(** A command argument *)
type simple_arg = CString of string | CIdent of string

type arg = simple_arg * filter option
(** Command argument *)

type command = arg list * filter option
(** Command *)

(** {2 Switches} *)

type switch = OpamSwitch.t
(** Compiler switches *)

type switch_set = OpamSwitch.Set.t
(** Set of compiler switches *)

type 'a switch_map = 'a OpamSwitch.Map.t
(** Map of compile switches *)

type switch_selections = {
  sel_installed : package_set;
  sel_roots : package_set;
  sel_compiler : package_set;
  sel_pinned : package_set;
}

(** {2 Misc} *)

(** The different kinds of locks *)
type lock =
  | Read_lock of (unit -> unit)
      (** The function does not modify anything, but it needs the state
      not to change while it is running. *)
  | Global_lock of (unit -> unit)
      (** Take the global lock, all subsequent calls to OPAM are
      blocked. *)
  | Switch_lock of (unit -> switch) * (unit -> unit)
      (** Take a global read lock and a switch lock. The first function is
      called with the read lock, then the second function is called with
      the returned switch write-locked. *)
  | Global_with_switch_cont_lock of (unit -> switch * (unit -> unit))
      (** Call the function in a global lock, then relax to a switch
      lock and call the function it returned *)

type file_attribute = OpamFilename.Attribute.t
(** A line in {i urls.tx} *)

type file_attribute_set = OpamFilename.Attribute.Set.t
(** All the lines in {i urls.txt} *)

type 'a optional = {
  c : 'a;  (** Contents *)
  optional : bool;  (** Is the contents optional *)
}
(** Optional contents *)

type stats = {
  s_install : int;
  s_reinstall : int;
  s_upgrade : int;
  s_downgrade : int;
  s_remove : int;
}
(** Upgrade statistics *)

type env = (string * string * string option) list
(** Environement variables: var name, value, optional comment *)

(** Environment updates *)
type env_update = string * env_update_op * string * string option
(** var, update_op, value, comment *)

type tags = OpamStd.String.Set.t OpamStd.String.SetMap.t
(** Tags *)

(** {2 Repository and global states} *)

type checksums = string list
(** Checksums *)

type json = OpamJson.t
(** {2 JSON} *)
