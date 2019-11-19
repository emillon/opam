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

(** Handles all OPAM file formats as record types and submodules, conversion to
    and from syntax *)

open OpamTypes

(** Functions to read and write OPAM configuration files in a typed way *)

type 'a t = private filename
(** Associate a type to a filename through a phantom type *)

type 'a typed_file = 'a t

val make : filename -> 'a t

val filename : 'a t -> filename

val to_string : 'a t -> string

val exists : 'a t -> bool

(** All Configuration files satisfy this signature *)
module type IO_FILE = sig
  type t
  (** File contents *)

  val format_version : OpamVersion.t

  val empty : t
  (** Empty file *)

  val write : t typed_file -> t -> unit
  (** Write some contents to a file *)

  val read : t typed_file -> t
  (** Read file contents. Raise an error if the file does not exist. *)

  val read_opt : t typed_file -> t option
  (** Returns [None] on non-existing file *)

  val safe_read : t typed_file -> t
  (** Read file contents. Return [empty] if the file does not exist. *)

  val read_from_channel : ?filename:t typed_file -> in_channel -> t

  val read_from_string : ?filename:t typed_file -> string -> t

  val write_to_channel : ?filename:t typed_file -> out_channel -> t -> unit

  val write_to_string : ?filename:t typed_file -> t -> string
end

module Lines : IO_FILE with type t = string list list
(** Lines of space-separated words. *)

(** Command wrappers for package scripts *)
module Wrappers : sig
  type t = {
    pre_build : command list;
    wrap_build : command list;
    post_build : command list;
    pre_install : command list;
    wrap_install : command list;
    post_install : command list;
    pre_remove : command list;
    wrap_remove : command list;
    post_remove : command list;
    pre_session : command list;
    post_session : command list;
  }

  val pre_build : t -> command list

  val wrap_build : t -> command list

  val post_build : t -> command list

  val pre_install : t -> command list

  val wrap_install : t -> command list

  val post_install : t -> command list

  val pre_remove : t -> command list

  val wrap_remove : t -> command list

  val post_remove : t -> command list

  val pre_session : t -> command list

  val post_session : t -> command list

  val empty : t

  val add : outer:t -> inner:t -> t
end

(** Configuration file: [$opam/config] *)
module Config : sig
  include IO_FILE

  val with_switch : switch -> t -> t
  (** OCaml switch updates *)

  val with_switch_opt : switch option -> t -> t

  val with_installed_switches : switch list -> t -> t

  val with_repositories : repository_name list -> t -> t
  (** Repository updates *)

  val with_opam_version : OpamVersion.t -> t -> t
  (** Update opam-version *)

  val with_criteria : (solver_criteria * string) list -> t -> t

  val with_best_effort_prefix : string -> t -> t

  val with_solver : arg list -> t -> t

  val with_solver_opt : arg list option -> t -> t

  val with_jobs : int -> t -> t

  val with_dl_tool : arg list -> t -> t

  val with_dl_tool_opt : arg list option -> t -> t

  val with_dl_jobs : int -> t -> t

  val with_dl_cache : url list -> t -> t

  val with_wrappers : Wrappers.t -> t -> t

  val with_global_variables :
    (variable * variable_contents * string) list -> t -> t

  val with_eval_variables : (variable * string list * string) list -> t -> t

  val with_validation_hook_opt : arg list option -> t -> t

  val with_default_compiler : formula -> t -> t

  val opam_version : t -> opam_version
  (** Return the OPAM version *)

  val repositories : t -> repository_name list
  (** Return the list of repository *)

  val switch : t -> switch option
  (** Return the OCaml switch *)

  val installed_switches : t -> switch list

  val jobs : t -> int option
  (** Return the number of jobs defined *)

  val dl_tool : t -> arg list option

  val dl_jobs : t -> int
  (** Return the number of download jobs *)

  val dl_cache : t -> url list

  val criteria : t -> (solver_criteria * string) list

  val best_effort_prefix : t -> string option

  val solver : t -> arg list option

  val wrappers : t -> Wrappers.t

  val global_variables : t -> (variable * variable_contents * string) list
  (** variable, value, docstring *)

  val eval_variables : t -> (variable * string list * string) list
  (** variable, command, docstring *)

  val validation_hook : t -> arg list option

  val default_compiler : t -> formula
end

(** Init config file [/etc/opamrc] *)
module InitConfig : sig
  include IO_FILE

  val opam_version : t -> opam_version

  val repositories : t -> (repository_name * (url * trust_anchors option)) list

  val default_compiler : t -> formula

  val jobs : t -> int option

  val dl_tool : t -> arg list option

  val dl_jobs : t -> int option

  val dl_cache : t -> url list

  val solver_criteria : t -> (solver_criteria * string) list

  val solver : t -> arg list option

  val wrappers : t -> Wrappers.t

  val global_variables : t -> (variable * variable_contents * string) list

  val eval_variables : t -> (variable * string list * string) list

  val recommended_tools :
    t -> (string list * string option * filter option) list

  val required_tools : t -> (string list * string option * filter option) list

  val init_scripts : t -> ((string * string) * filter option) list

  val with_opam_version : opam_version -> t -> t

  val with_repositories :
    (repository_name * (url * trust_anchors option)) list -> t -> t

  val with_default_compiler : formula -> t -> t

  val with_jobs : int option -> t -> t

  val with_dl_tool : arg list option -> t -> t

  val with_dl_jobs : int option -> t -> t

  val with_dl_cache : url list -> t -> t

  val with_solver_criteria : (solver_criteria * string) list -> t -> t

  val with_solver : arg list option -> t -> t

  val with_wrappers : Wrappers.t -> t -> t

  val with_global_variables :
    (variable * variable_contents * string) list -> t -> t

  val with_eval_variables : (variable * string list * string) list -> t -> t

  val with_recommended_tools :
    (string list * string option * filter option) list -> t -> t

  val with_required_tools :
    (string list * string option * filter option) list -> t -> t

  val with_init_scripts : ((string * string) * filter option) list -> t -> t

  val add : t -> t -> t
  (** [add t1 t2] is [t2], with the field values falling back to those of [t1]
      when not set in [t2] *)
end

(** Package descriptions: [$opam/descr/] *)
module Descr : sig
  include IO_FILE

  val create : string -> t

  val of_string : t typed_file -> string -> t
  (** Create an abstract description file from a string *)

  val synopsis : t -> string
  (** Return the first line *)

  val body : t -> string
  (** Return the body *)

  val full : t -> string
  (** Return the full description *)
end

(** {2 Urls for OPAM repositories} *)
module URL : sig
  include IO_FILE

  val create : ?mirrors:url list -> ?checksum:OpamHash.t list -> url -> t

  val url : t -> url
  (** URL address *)

  val mirrors : t -> url list

  val checksum : t -> OpamHash.t list
  (** Archive checksum *)

  val with_checksum : OpamHash.t list -> t -> t
  (** Constructor *)
end

(** OPAM files *)
module OPAM : sig
  type t = private {
    opam_version : opam_version;
    (* Package ident *)
    name : name option;
    version : version option;
    (* Relationships; solver and availability info *)
    depends : filtered_formula;
    depopts : filtered_formula;
    conflicts : filtered_formula;
    conflict_class : name list;
    available : filter;
    flags : package_flag list;
    env : env_update list;
    (* Build instructions *)
    build : command list;
    run_test : command list;
    install : command list;
    remove : command list;
    (* Auxiliary data affecting the build *)
    substs : basename list;
    patches : (basename * filter option) list;
    build_env : env_update list;
    features : (OpamVariable.t * filtered_formula * string) list;
    extra_sources : (basename * URL.t) list;
    (* User-facing data used by opam *)
    messages : (string * filter option) list;
    post_messages : (string * filter option) list;
    depexts : (string list * filter) list;
    libraries : (string * filter option) list;
    syntax : (string * filter option) list;
    dev_repo : url option;
    pin_depends : (package * url) list;
    (* Package database details *)
    maintainer : string list;
    author : string list;
    license : string list;
    tags : string list;
    homepage : string list;
    doc : string list;
    bug_reports : string list;
    (* Extension fields (x-foo: "bar") *)
    extensions : (pos * value) OpamStd.String.Map.t;
    (* Extra sections *)
    url : URL.t option;
    descr : Descr.t option;
    (* Related metadata directory (not an actual field of the file)
       This can be used to locate e.g. the files/ overlays.
       If the repository is specified, the string is a relative path from its
       root. It should otherwise be an absolute path. *)
    metadata_dir : (repository_name option * string) option;
    (* Names and hashes of the files below files/ *)
    extra_files : (OpamFilename.Base.t * OpamHash.t) list option;
    format_errors : (string * OpamPp.bad_format) list;
    (* Deprecated, for compat and proper linting *)
    ocaml_version : (OpamFormula.relop * string) OpamFormula.formula option;
    os : (bool * string) generic_formula;
    deprecated_build_test : command list;
    deprecated_build_doc : command list;
  }

  include IO_FILE with type t := t

  val empty : t

  val create : package -> t
  (** Create an opam file *)

  val effective_part : t -> t
  (** Returns the opam value (including url, descr) with all non-effective (i.e.
      user-directed information that doesn't change opam's view on the package)
      fields set to their empty values. Useful for comparisons. *)

  val effectively_equal : t -> t -> bool
  (** Returns true if the effective parts of the two package definitions are
      equal *)

  val equal : t -> t -> bool
  (** Compares two package definitions, ignoring the virtual fields bound to
      file location ([metadata_dir]...) *)

  val print_errors : ?file:t typed_file -> t -> unit
  (** Prints the format errors that were found when the file was read *)

  val opam_version : t -> opam_version
  (** Get OPAM version. *)

  val name : t -> name
  (** Package name *)

  val name_opt : t -> name option

  val version : t -> version
  (** Package version *)

  val version_opt : t -> version option

  val package : t -> package
  (** The informations in both the name and version fields, as a package *)

  val available : t -> filter
  (** Availability formula (OS + compiler constraints) *)

  val maintainer : t -> string list
  (** Package maintainer(s) *)

  val substs : t -> basename list
  (** File substitutions *)

  val build_env : t -> env_update list
  (** List of environment variables to set-up for the build *)

  val build : t -> command list
  (** List of command to run for building the package *)

  val install : t -> command list
  (** List of command to run for installing the package *)

  val remove : t -> command list
  (** List of command to run for removing the package *)

  val depends : t -> filtered_formula
  (** Package dependencies *)

  val depopts : t -> filtered_formula
  (** Optional dependencies *)

  val depexts : t -> (string list * filter) list
  (** External dependencies *)

  val extra_sources : t -> (basename * URL.t) list

  val extensions : t -> value OpamStd.String.Map.t
  (** All extended "x-" fields as a map *)

  val extended : t -> string -> (value -> 'a) -> 'a option
  (** Parse a single extended field (reports proper file position) *)

  val with_messages : (string * filter option) list -> t -> t

  val with_post_messages : (string * filter option) list -> t -> t

  val conflicts : t -> filtered_formula
  (** Package conflicts *)

  val conflict_class : t -> name list

  val features : t -> (OpamVariable.t * filtered_formula * string) list
  (** Contents of the 'features' field *)

  val libraries : t -> (string * filter option) list
  (** List of exported libraries *)

  val syntax : t -> (string * filter option) list
  (** List of exported syntax extensions *)

  val patches : t -> (basename * filter option) list
  (** Patches *)

  val homepage : t -> string list
  (** Homepage(s) *)

  val author : t -> string list
  (** Author(s) *)

  val license : t -> string list
  (** License(s) *)

  val doc : t -> string list
  (** API documentation *)

  val tags : t -> string list
  (** Classification tags *)

  val run_test : t -> command list
  (** Commands to build and run the tests *)

  val deprecated_build_doc : t -> command list
  (** Commands to build the documentation *)

  val deprecated_build_test : t -> command list
  (** Commands to build the tests *)

  val messages : t -> (string * filter option) list
  (** Messages to display before taking action *)

  val post_messages : t -> (string * filter option) list
  (** Messages to display at end of install *)

  val bug_reports : t -> string list
  (** Where to post bug reports. *)

  val flags : t -> package_flag list
  (** The package flags that are present for this package. *)

  val has_flag : package_flag -> t -> bool
  (** Check the package for the given flag. Allows flags specified through tags
      for compatibility *)

  val env : t -> env_update list
  (** The environment variables that this package exports *)

  val descr : t -> Descr.t option

  val synopsis : t -> string option

  val descr_body : t -> string option

  val url : t -> URL.t option

  val get_url : t -> url option

  val metadata_dir : t -> (repository_name option * string) option
  (** Related metadata directory (either repository name + relative path, or
      absolute path; not an actual field of the file, linked to the file
      location).
      This can be used to locate e.g. the files/ overlays *)

  val get_metadata_dir :
    repos_roots:(repository_name -> dirname) -> t -> dirname option
  (** Gets the resolved metadata dir, given a mapping of repository names to
      their roots *)

  val extra_files : t -> (OpamFilename.Base.t * OpamHash.t) list option
  (** Names and hashes of the files below files/ *)

  val get_extra_files :
    repos_roots:(repository_name -> dirname) ->
    t ->
    (filename * basename * OpamHash.t) list
  (** Looks up the extra files, and returns their full paths, relative path to
      the package source, and hash. Doesn't check the hashes. *)

  val format_errors : t -> (string * OpamPp.bad_format) list
  (** Returns the errors that were found when parsing the file, associated to
      their fields (that were consequently ignored) *)

  val with_opam_version : opam_version -> t -> t
  (** Sets the opam version *)

  val dev_repo : t -> url option
  (** The package source repository address *)

  val pin_depends : t -> (package * url) list

  val with_name : name -> t -> t
  (** construct as [name] *)

  val with_name_opt : name option -> t -> t

  val with_version : version -> t -> t
  (** construct as [version] *)

  val with_version_opt : version option -> t -> t

  val with_depends : filtered_formula -> t -> t
  (** Construct as [depends] *)

  val with_depopts : filtered_formula -> t -> t
  (** Construct as [depopts] *)

  val with_conflicts : filtered_formula -> t -> t

  val with_conflict_class : name list -> t -> t

  val with_features :
    (OpamVariable.t * filtered_formula * string) list -> t -> t

  val with_build : command list -> t -> t
  (** Construct as [build] *)

  val with_run_test : command list -> t -> t

  val with_install : command list -> t -> t

  val with_remove : command list -> t -> t
  (** Construct as [remove] *)

  val with_libraries : (string * filter option) list -> t -> t
  (** Construct as [libraries] *)

  val with_syntax : (string * filter option) list -> t -> t
  (** Replace the [syntax] field of the given OPAM file. *)

  val with_substs : basename list -> t -> t
  (** Construct as [substs] *)

  val with_build_env : env_update list -> t -> t

  val with_available : filter -> t -> t

  val with_maintainer : string list -> t -> t
  (** Construct as [maintainer] *)

  val with_author : string list -> t -> t

  val with_homepage : string list -> t -> t

  val with_license : string list -> t -> t

  val with_patches : (basename * filter option) list -> t -> t
  (** Construct as [patches] *)

  val with_bug_reports : string list -> t -> t
  (** Construct using [bug_reports] *)

  val with_depexts : (string list * filter) list -> t -> t
  (** Construct using [depexts] *)

  val with_flags : package_flag list -> t -> t

  val add_flags : package_flag list -> t -> t

  val with_tags : string list -> t -> t

  val with_env : env_update list -> t -> t

  val with_dev_repo : url -> t -> t

  val with_dev_repo_opt : url option -> t -> t

  val with_pin_depends : (package * url) list -> t -> t

  val with_extra_sources : (basename * URL.t) list -> t -> t

  val with_extensions : value OpamStd.String.Map.t -> t -> t

  val add_extension : t -> string -> value -> t

  val with_deprecated_build_doc : command list -> t -> t

  val with_deprecated_build_test : command list -> t -> t

  val with_descr : Descr.t -> t -> t

  val with_descr_opt : Descr.t option -> t -> t

  val with_synopsis : string -> t -> t

  val with_descr_body : string -> t -> t
  (** If [synopsis] is not already set, split the string and use the first line
      as synopsis. *)

  val with_url : URL.t -> t -> t

  val with_url_opt : URL.t option -> t -> t

  val with_metadata_dir : (repository_name option * string) option -> t -> t

  val with_extra_files : (OpamFilename.Base.t * OpamHash.t) list -> t -> t

  val with_extra_files_opt :
    (OpamFilename.Base.t * OpamHash.t) list option -> t -> t

  val with_format_errors : (string * OpamPp.bad_format) list -> t -> t

  val to_string_with_preserved_format :
    ?format_from:t typed_file ->
    ?format_from_string:string ->
    t typed_file ->
    t ->
    string
  (** Prints to a string, while keeping the format of the original file as much
      as possible. The original format is read from the given
      [format_from_string], the file [format_from], or the output file if
      it exists *)

  val write_with_preserved_format :
    ?format_from:t typed_file ->
    ?format_from_string:string ->
    t typed_file ->
    t ->
    unit
  (** Writes an opam file, but preserving the existing formatting as much as
      possible. The original format is read from the given
      [format_from_string], the file [format_from], or the output file if
      it exists *)

  (** Low-level values used for linting and similar processing *)

  val flag_of_tag : string -> package_flag option
  (** Allow 'flag:xxx' tags as flags, for compat *)

  val fields : (t, value) OpamFormat.I.fields_def

  val sections :
    (t, (string option * opamfile_item list) list) OpamFormat.I.fields_def

  val pp_raw_fields : (opamfile_item list, t) OpamPp.t
  (** Doesn't handle package name encoded in directory name *)

  val contents : ?filename:'a typed_file -> t -> opamfile
  (** Returns the raw print-AST contents of the file *)

  val to_list : ?filename:'a typed_file -> t -> (string * value) list
  (** Returns all fields of the file as print-AST. Fields within sections are
      accessed through dot-separated paths (e.g. [url.checksum]) *)

  val print_field_as_syntax : string -> t -> value option
  (** Gets the print-AST for a single field in the file structure. Fields within
      sections can be accessed through [section.field]. *)
end

module Aliases : IO_FILE with type t = string switch_map
(** Compiler aliases: [$opam/aliases]. Deprecated, used only for migration *)

(** Switch state file as table, also used for import/export. This includes
    compiler and root packages information, as well as pinned packages and their
    target (but not their local metadata). *)
module LegacyState : sig
  type t = switch_selections

  include IO_FILE with type t := t
end

(** A newer format for switch state, using the opam file syntax rather than a
    table. This is more readable and extensible. *)
module SwitchSelections : sig
  type t = switch_selections

  include IO_FILE with type t := t
end

(** An extended version of SwitchSelections that can include full opam files as
    [package "name" {}] sections, for storing overlays *)
module SwitchExport : sig
  type t = {
    selections : switch_selections;
    overlays : OPAM.t OpamPackage.Name.Map.t;
  }

  include IO_FILE with type t := t
end

module PkgList : IO_FILE with type t = package_set
(** A simple list of packages and versions: (used for the older
    [$opam/$switch/{installed,installed_roots}], still needed to
    migrate from 1.2 repository, and for reinstall) *)

module Environment : IO_FILE with type t = env_update list
(** Cached environment updates (<switch>/environment) *)

(** Compiler version [$opam/compilers/]. Deprecated, only used to upgrade old
    data *)
module Comp : sig
  include IO_FILE

  type compiler = string

  type compiler_version = string

  val create_preinstalled :
    compiler -> compiler_version -> name list -> env_update list -> t
  (** Create a pre-installed compiler description file *)

  val preinstalled : t -> bool
  (** Is it a pre-installed compiler description file *)

  val opam_version : t -> opam_version
  (** Get OPAM version *)

  val name : t -> compiler
  (** Return the compiler name *)

  val version : t -> compiler_version
  (** Return the compiler version *)

  val src : t -> url option
  (** Return the url of the compiler *)

  val patches : t -> url list
  (** Return the list of patches to apply *)

  val configure : t -> string list
  (** Options to give to the "./configure" command *)

  val make : t -> string list
  (** Options to give to the "make" command *)

  val build : t -> command list
  (** Options to give to build the package. If this one is provided,
      nothing should be specified for [configure] and [make]. *)

  val packages : t -> formula
  (** Packages to install immediately after the creation of OCaml *)

  val env : t -> env_update list
  (** Environment variable to set-up before running commands in the
      subtree *)

  val tags : t -> string list

  val with_src : url option -> t -> t

  val with_patches : url list -> t -> t

  val with_configure : string list -> t -> t

  val with_make : string list -> t -> t

  val with_build : command list -> t -> t

  val with_packages : formula -> t -> t

  val to_package : ?package:package -> t -> Descr.t option -> OPAM.t
  (** Converts a compiler definition to package metadata. For compat. If
      [package] is unspecified, a package named "ocaml" is created for
      "standard" compilers (when the compiler name doesn't contain a "+" and is
      equal to the compiler version); otherwise, a package "ocaml-VARIANT" is
      created with "VARIANT" the part of the compiler name on the right of the
      "+". In both case, the version corresponds to the OCaml version and is
      [version comp]. *)
end

(** {2 Configuration files} *)

(** .install files *)
module Dot_install : sig
  include IO_FILE

  val bin : t -> (basename optional * basename option) list
  (** List of files to install in $bin/ *)

  val sbin : t -> (basename optional * basename option) list
  (** List of files to install in $sbin/ *)

  val lib : t -> (basename optional * basename option) list
  (** List of files to install in $lib/ *)

  val toplevel : t -> (basename optional * basename option) list
  (** List of toplevel files *)

  val stublibs : t -> (basename optional * basename option) list
  (** C bindings *)

  val share : t -> (basename optional * basename option) list
  (** List of architecture-independent files *)

  val share_root : t -> (basename optional * basename option) list
  (** List of files under the more general share prefix *)

  val etc : t -> (basename optional * basename option) list
  (** List of etc files *)

  val doc : t -> (basename optional * basename option) list
  (** List of doc files *)

  val man : t -> (basename optional * basename option) list
  (** Man pages *)

  val libexec : t -> (basename optional * basename option) list
  (** Executable files under lib/ *)

  val lib_root : t -> (basename optional * basename option) list
  (** Not relative to the package's lib dir *)

  val libexec_root : t -> (basename optional * basename option) list
  (** Not relative to the package's lib dir, and with +x set *)

  val misc : t -> (basename optional * filename) list
  (** List of other files to install *)
end

(** .changes files, bound to the OpamDirTrack module *)
module Changes : sig
  type t = OpamDirTrack.t

  include IO_FILE with type t := t
end

(** .config files *)
module Dot_config : sig
  include IO_FILE

  val create : (variable * variable_contents) list -> t
  (** Create a new .config file (containing only variables) *)

  val file_depends : t -> (filename * OpamHash.t) list
  (** Dependency towards file-system paths and their hashes *)

  val with_file_depends : (filename * OpamHash.t) list -> t -> t

  val with_vars : (variable * variable_contents) list -> t -> t
  (** Sets all bindings in the file *)

  val variable : t -> variable -> variable_contents option
  (** Top-level variables *)

  val variables : t -> variable list
  (** The list of top-level variables *)

  val bindings : t -> (variable * variable_contents) list
  (** Lists all the variable bindings in the file *)

  val set : variable -> variable_contents option -> t -> t
  (** Sets the given variable, overriding any previous definition.
      With [None], unsets the variable*)
end

(** {2 Repository files} *)

module Package_index :
  IO_FILE with type t = (repository_name * string option) package_map
(** Association between package names and repositories *)

(** Repository config: [$opam/repo/$repo/config]. Deprecated, for migration
    only *)
module Repo_config_legacy : sig
  type t = {
    repo_name : repository_name;
    repo_root : dirname;
    repo_url : url;
    repo_priority : int;
  }

  include IO_FILE with type t := t
end

module Repos_config :
  IO_FILE
    with type t = (url * trust_anchors option) option OpamRepositoryName.Map.t

module Switch_config : sig
  type t = {
    opam_version : OpamVersion.t;
    synopsis : string;
    repos : repository_name list option;
    paths : (std_path * string) list;
    variables : (variable * variable_contents) list;
    opam_root : dirname option;
    wrappers : Wrappers.t;
    env : env_update list;
  }

  val variable : t -> variable -> variable_contents option

  val path : t -> std_path -> string option

  val wrappers : t -> Wrappers.t

  include IO_FILE with type t := t
end

(** Pinned package files (only used for migration from 1.2, the inclusive State
    module is now used instead) *)
module Pinned_legacy : sig
  type pin_option = Version of version | Source of url

  include IO_FILE with type t = pin_option name_map
end

(** Repository metadata *)
module Repo : sig
  include IO_FILE

  val create :
    ?browse:string ->
    ?upstream:string ->
    ?opam_version:OpamVersion.t ->
    ?redirect:(string * filter option) list ->
    ?root_url:url ->
    ?dl_cache:string list ->
    ?announce:(string * filter option) list ->
    ?stamp:string ->
    unit ->
    t

  val opam_version : t -> OpamVersion.t option
  (** The minimum OPAM version required for this repository, if defined *)

  val browse : t -> string option
  (** Base URL for browsing packages on the WWW *)

  val upstream : t -> string option
  (** Base URL for browsing OPAM repository source on the WWW *)

  val root_url : t -> url option
  (** The root URL of the repository (not an actual file field, determined at
      runtime by opam) *)

  val redirect : t -> (string * filter option) list
  (** Redirections. *)

  val dl_cache : t -> string list
  (** Cache URLs, either full or relative to the repo root *)

  val announce : t -> (string * filter option) list

  val stamp : t -> string option

  val with_opam_version : OpamVersion.t -> t -> t

  val with_browse : string -> t -> t

  val with_upstream : string -> t -> t

  val with_redirect : (string * filter option) list -> t -> t

  val with_root_url : url -> t -> t

  val with_dl_cache : string list -> t -> t

  val with_announce : (string * filter option) list -> t -> t

  val with_stamp : string -> t -> t

  val with_stamp_opt : string option -> t -> t
end

module File_attributes : IO_FILE with type t = file_attribute_set
(** {2 urls.txt file *} *)

module Stats : sig
  val print : unit -> unit
  (** Display statistics about file access. *)
end

(** Helper module for manipulation of the raw syntax ([opamfile]) format.
    (the specific file handling modules are derived from this one) *)
module Syntax : sig
  val pp_channel :
    'a typed_file -> in_channel -> out_channel -> (unit, opamfile) OpamPp.t

  val of_channel : 'a typed_file -> in_channel -> opamfile

  val to_channel : 'a typed_file -> out_channel -> opamfile -> unit

  val of_string : 'a typed_file -> string -> opamfile

  val to_string : 'a typed_file -> opamfile -> string

  val to_string_with_preserved_format :
    'a typed_file ->
    ?format_from:'a typed_file ->
    ?format_from_string:string ->
    empty:'a ->
    ?sections:
      ('a, (string option * opamfile_item list) list) OpamFormat.I.fields_def ->
    fields:('a, value) OpamFormat.I.fields_def ->
    (opamfile, filename * 'a) OpamPp.t ->
    'a ->
    string
end

(**/**)

module type SyntaxFileArg = sig
  val internal : string

  val format_version : OpamVersion.t

  type t

  val empty : t

  val pp : (opamfile, filename * t) OpamPp.t
end

module SyntaxFile (X : SyntaxFileArg) : IO_FILE with type t := X.t

module type LineFileArg = sig
  val internal : string

  type t

  val empty : t

  val pp : (string list list, t) OpamPp.t
end

module LineFile (X : LineFileArg) : IO_FILE with type t := X.t
