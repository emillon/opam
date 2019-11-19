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

(** Defines the file hierarchy in ~/.opam *)

open OpamTypes

(** {2 Global paths} *)

type t = dirname
(** Type of path root *)

val state_cache : t -> filename
(** State cache *)

val lock : t -> filename
(** Global lock file for the whole opamroot. Opam should generally read-lock
    this (e.g. initialisation and format upgrades require a write lock) *)

val config : t -> OpamFile.Config.t OpamFile.t
(** Main configuration file: {i $opam/config} *)

val init_config_files : unit -> OpamFile.InitConfig.t OpamFile.t list
(** The list of configuration files location used by default ({i /etc/opamrc}
    and {i ~/.opamrc}). More general (lower priority) first. *)

val config_lock : t -> filename
(** Lock for updates on the main config file (write lock when changes to
    switches, repositories lists are expected. No lock needed otherwise) *)

val archives_dir : t -> dirname
(** Archives dir *)

val archive : t -> package -> filename
(** Archive file: {i $opam/archives/$NAME.$VERSION+opam.tar.gz} *)

val repos_lock : t -> filename
(** Global lock file for the repositories mirrors: {i $opam/repo/lock} *)

val repos_config : t -> OpamFile.Repos_config.t OpamFile.t
(** Global config file for the repositories mirrors:
    {i $opam/repo/repos-config} *)

val init : t -> dirname
(** Init scripts location: {i $opam/opam-init} *)

val hooks_dir : t -> dirname
(** Installation dir for configured hooks: ${i $opam/opam-init/hooks} *)

val log : t -> dirname
(** Log dir {i $opam/log} *)

val backup_dir : t -> dirname
(** The directory where global backups are stored *)

val backup : t -> switch_selections OpamFile.t
(** Backup file for state export *)

val plugins : t -> dirname
(** The directory for plugins data {i $opam/plugins} *)

val plugins_bin : t -> dirname
(** The directory for shared plugin binaries {i $opam/plugins/bin} *)

val plugin_bin : t -> name -> filename
(** The globally installed binary of the given plugin {i
    $opam/plugins/bin/opam-foo} *)

val plugin : t -> name -> dirname
(** The directory for a given plugin's data {i $opam/plugins/$name}. ["bin"] is
    forbidden. *)

(** Switch related paths *)
module Switch : sig
  (** Locations of opam internal dirs and files *)

  val root : t -> switch -> dirname
  (** The switch prefix: {i $opam/$switch} *)

  val meta_dirname : string
  (** The name of the subdir of the switch prefix where opam data is stored
      (".opam-switch") *)

  val meta : t -> switch -> dirname
  (** The subdirectory of the prefix where opam data lives:
      {i $opam/$switch/.opam-switch}*)

  val lock : t -> switch -> filename
  (** lock file: {i $meta/lock} *)

  val backup_dir : t -> switch -> dirname
  (** The directory where backups are stored for this switch *)

  val backup : t -> switch -> switch_selections OpamFile.t
  (** Backup file for state export *)

  val selections : t -> switch -> switch_selections OpamFile.t
  (** Switch selections {i $meta/switch-state} *)

  val build : t -> switch -> package -> dirname
  (** Temporary folders used to decompress and compile
      the corresponding archives:
      {i $meta/build/$packages} *)

  val remove : t -> switch -> package -> dirname
  (** Temporary folders used to decompress the corresponding archives, used only
      for package removal {i $meta/remove/$packages} *)

  val build_dir : t -> switch -> dirname
  (** Temporary folder: {i $meta/build} *)

  val remove_dir : t -> switch -> dirname
  (** Temporary folder: {i $meta/remove} *)

  val install : t -> switch -> name -> OpamFile.Dot_install.t OpamFile.t
  (** Installed files for a given package: {i
      $meta/install/$name.install} *)

  val changes : t -> switch -> name -> OpamDirTrack.t OpamFile.t
  (** File registering the changes made by the installation of the given package
      {i $meta/install/$name.changes} *)

  val install_dir : t -> switch -> dirname
  (** Installed files: {i $meta/install/} *)

  val reinstall : t -> switch -> OpamFile.PkgList.t OpamFile.t
  (** Packages to reinstall on next upgrade: {i
      $meta/reinstall} *)

  val config_dir : t -> switch -> dirname
  (** Configuration folder: {i $meta/config} *)

  val switch_config : t -> switch -> OpamFile.Switch_config.t OpamFile.t
  (** Global config for the switch: {i $meta/switch-config} *)

  val config : t -> switch -> name -> OpamFile.Dot_config.t OpamFile.t
  (** Package-specific configuration file for installed packages: {i
      $meta/config/$name.config} *)

  val sources_dir : t -> switch -> dirname
  (** Clean, uncompressed sources for this switch: {i $meta/sources/} *)

  val sources : t -> switch -> package -> dirname
  (** Clean, uncompressed source directory for this package: {i
      $meta/sources/$name.$version/} *)

  val pinned_package : t -> switch -> name -> dirname
  (** Mirror of the sources for a given pinned package: {i
      $meta/sources/$name/} (without version) *)

  val environment : t -> switch -> OpamFile.Environment.t OpamFile.t
  (** Cached environment updates. *)

  val env_relative_to_prefix : dirname -> OpamFile.Environment.t OpamFile.t
  (** Like [environment], but from the switch prefix dir *)

  val installed_opams : t -> switch -> dirname
  (** Directory where the metadata of installed packages is mirrored.
      {i $meta/packages/} *)

  val installed_package_dir : t -> switch -> package -> dirname
  (** The mirror of the package definition for the given installed package {i
      $meta/packages/$name.$version/} *)

  val installed_opam : t -> switch -> package -> OpamFile.OPAM.t OpamFile.t
  (** The mirror of the opam file for the given installed package
      {i $meta/packages/$name.$version/opam} *)

  val installed_opam_files_dir : t -> switch -> package -> dirname
  (** Mirror of the extra files attached to the package definitions of installed
      packages
      {i $meta/packages/$name.$version/files/} *)

  (** Locations for the visible part of the installation *)

  (** Default config *)
  module Default : sig
    val lib : t -> switch -> name -> dirname
    (** Library path for a given package:
        {i $prefix/lib/$name} *)

    val lib_dir : t -> switch -> dirname
    (** Library path: {i $prefix/lib} *)

    val stublibs : t -> switch -> dirname
    (** DLL paths *)

    val toplevel : t -> switch -> dirname
    (** toplevel path: {i $prefix/lib/toplevel} *)

    val doc : t -> switch -> name -> dirname
    (** Documentation path for a given package:
        {i $prefix/doc/$name} *)

    val doc_dir : t -> switch -> dirname
    (** Documentation path: {i $prefix/doc/} *)

    val share_dir : t -> switch -> dirname
    (** Shared directory: {i $prefix/share} *)

    val share : t -> switch -> name -> dirname
    (** Share directory for a given package: {i
        $prefix/share/$package} *)

    val etc_dir : t -> switch -> dirname
    (** Etc directory: {i $prefix/etc} *)

    val etc : t -> switch -> name -> dirname
    (** Etc directory for a given package: {i
        $prefix/etc/$package} *)

    val man_dir : ?num:string -> t -> switch -> dirname
    (** Man pages path: {i $prefix/man/}. The optional
        [num] argument will add a {i manN } suffix if specified *)

    val bin : t -> switch -> dirname
    (** Installed binaries: {i $prefix/bin} *)

    val sbin : t -> switch -> dirname
    (** Installed system binaries: {i $prefix/sbin} *)
  end

  (** Actual config handling the global-config.config indirections *)

  (** Package-independent dirs *)

  val get_stdpath :
    t -> switch -> OpamFile.Switch_config.t -> std_path -> dirname

  val lib_dir : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Library path: {i $prefix/lib} *)

  val stublibs : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** DLL paths *)

  val toplevel : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** toplevel path: {i $prefix/lib/toplevel} *)

  val doc_dir : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Documentation path: {i $prefix/doc/} *)

  val share_dir : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Shared directory: {i $prefix/share} *)

  val etc_dir : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Etc directory: {i $prefix/etc} *)

  val man_dir :
    ?num:string -> t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Man pages path: {i $prefix/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)

  val bin : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Installed binaries: {i $prefix/bin} *)

  val sbin : t -> switch -> OpamFile.Switch_config.t -> dirname
  (** Installed system binaries: {i $prefix/sbin} *)

  (** Package dependent dirs *)

  val lib : t -> switch -> OpamFile.Switch_config.t -> name -> dirname
  (** Library path for a given package:
      {i $prefix/lib/$name} *)

  val doc : t -> switch -> OpamFile.Switch_config.t -> name -> dirname
  (** Documentation path for a given package:
      {i $prefix/doc/$name} *)

  val share : t -> switch -> OpamFile.Switch_config.t -> name -> dirname
  (** Share directory for a given package: {i
      $prefix/share/$package} *)

  val etc : t -> switch -> OpamFile.Switch_config.t -> name -> dirname
  (** Etc directory for a given package: {i
      $prefix/etc/$package} *)

  module Overlay : sig
    val dir : t -> switch -> dirname
    (** Switch metadata overlay (over the global metadata): {i
        $meta/overlay/} *)

    val package : t -> switch -> name -> dirname
    (** Switch metadata overlay (over the global metadata): {i
        $meta/overlay/$name.$version} *)

    val opam : t -> switch -> name -> OpamFile.OPAM.t OpamFile.t
    (** OPAM overlay: {i
        $meta/overlay/$name.$version/opam} *)

    val tmp_opam : t -> switch -> name -> OpamFile.OPAM.t OpamFile.t
    (** OPAM temp overlay (for user editing): {i
        $meta/overlay/$name.$version/opam_} *)

    val url : t -> switch -> name -> OpamFile.URL.t OpamFile.t
    (** URL overlay: {i
        $meta/overlay/$name.$version/url} *)

    val descr : t -> switch -> name -> OpamFile.Descr.t OpamFile.t
    (** Descr orverlay *)

    val files : t -> switch -> name -> dirname
    (** Files overlay *)
  end
end

(** Location of package-specific files relative to their build directory *)
module Builddir : sig
  val install : dirname -> package -> OpamFile.Dot_install.t OpamFile.t
  (** package.install file: {i $builddir/$name.install} *)

  val config : dirname -> package -> OpamFile.Dot_config.t OpamFile.t
  (** package.config file: {i $builddir/$name.config} *)
end
