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

(** Higher level file and directory name manipulation AND file operations,
    wrappers on OpamSystem using the filename type *)

(** Basenames *)
module Base : sig
  include OpamStd.ABSTRACT

  val check_suffix : t -> string -> bool
  (** Check whether a basename has a given suffix *)

  val add_extension : t -> string -> t
  (** Add a file extension *)
end

module Dir : OpamStd.ABSTRACT
(** Directory names *)

val cwd : unit -> Dir.t
(** Return the current working directory *)

val rmdir : Dir.t -> unit
(** Remove a directory *)

val cleandir : Dir.t -> unit
(** Cleans the contents of a directory, but keeps the directory in place. *)

val rmdir_cleanup : Dir.t -> unit
(** Removes an empty directory, as well as any empty leading path components *)

val mkdir : Dir.t -> unit
(** Create a directory *)

val rec_dirs : Dir.t -> Dir.t list
(** List the sub-directory recursively *)

val dir_is_empty : Dir.t -> bool

val dirs : Dir.t -> Dir.t list
(** List the sub-directory (do not recurse) *)

val in_dir : Dir.t -> (unit -> 'a) -> 'a
(** Evaluate a function in a given directory *)

val env_of_list : (string * string) list -> string array
(** Turns an assoc list into an array suitable to be provided as environment *)

val exec :
  Dir.t ->
  ?env:(string * string) list ->
  ?name:string ->
  ?metadata:(string * string) list ->
  ?keep_going:bool ->
  string list list ->
  unit
(** Execute a list of commands in a given directory *)

val move_dir : src:Dir.t -> dst:Dir.t -> unit
(** Move a directory *)

val copy_dir : src:Dir.t -> dst:Dir.t -> unit
(** Copy directory [src] to [dst], that is, recursively copy the contents of
    [src] into [dst], overwriting any existing files. *)

val link_dir : target:Dir.t -> link:Dir.t -> unit
(** Link a directory *)

val exists_dir : Dir.t -> bool
(** Does the directory exist? *)

val opt_dir : Dir.t -> Dir.t option
(** Returns the argument as option, if the directory exists *)

val dirname_dir : Dir.t -> Dir.t
(** Return the parent directory *)

val basename_dir : Dir.t -> Base.t
(** Return the deeper directory name *)

val to_list_dir : Dir.t -> Dir.t list
(** Turn a full path into a list of directory names *)

val raw_dir : string -> Dir.t
(** Creation from a raw string, without resolving symlinks etc. *)

val with_tmp_dir : (Dir.t -> 'a) -> 'a
(** Execute a function in a temp directory *)

val with_tmp_dir_job : (Dir.t -> 'a OpamProcess.job) -> 'a OpamProcess.job
(** Provide an automatically cleaned up temp directory to a job *)

val mk_tmp_dir : unit -> Dir.t
(** Raw function to create a temporary directory. No automatic cleanup *)

val concat_and_resolve : Dir.t -> string -> Dir.t
(** Create a new Dir.t and resolve symlinks *)

include OpamStd.ABSTRACT

(** Generic filename *)
type generic_file = D of Dir.t | F of t

val create : Dir.t -> Base.t -> t
(** Create a filename from a Dir.t and a basename *)

val of_basename : Base.t -> t
(** Create a file from a basename and the current working directory
    as dirname *)

val raw : string -> t
(** Creation from a raw string, without resolving symlinks, etc. *)

val prettify : t -> string
(** Prettify a filename:
    - replace /path/to/opam/foo by <opam>/foo
    - replace /path/to/home/foo by ~/foo *)

val prettify_dir : Dir.t -> string
(** Prettify a dirname. *)

val dirname : t -> Dir.t
(** Return the directory name *)

val basename : t -> Base.t
(** Return the base name *)

val read : t -> string
(** Retrieves the contents from the hard disk. *)

val open_in : t -> in_channel
(** Open a channel from a given file. *)

val open_in_bin : t -> in_channel

val open_out : t -> out_channel

val open_out_bin : t -> out_channel

val remove : t -> unit
(** Removes everything in [filename] if existed. *)

val write : t -> string -> unit
(** Removes everything in [filename] if existed, then write [contents] instead. *)

val exists : t -> bool
(** Returns true if the file exists and is a regular file or a symlink to one *)

val opt_file : t -> t option
(** Returns the argument as option if it exists and is either a regular file or
    a symlink to one *)

val check_suffix : t -> string -> bool
(** Check whether a file has a given suffix *)

val add_extension : t -> string -> t
(** Adds a dot and the given file extension *)

val chop_extension : t -> t
(** Remove the file extension *)

val rec_files : Dir.t -> t list
(** List all the filenames, recursively *)

val files : Dir.t -> t list
(** List all the filename. Do not recurse. *)

val with_contents : (string -> 'a) -> t -> 'a
(** Apply a function on the contents of a file *)

val copy_in : ?root:Dir.t -> t -> Dir.t -> unit
(** Copy a file in a directory. If [root] is set, copy also the
    sub-directories. For instance, [copy_in ~root:"/foo" "/foo/bar/gni"
    "/toto"] creates ["/toto/bar/gni"]. *)

val move : src:t -> dst:t -> unit
(** Move a file *)

val readlink : t -> t
(** Read a symlinked file *)

val is_symlink : t -> bool
(** Is a symlink? *)

val is_symlink_dir : Dir.t -> bool

val is_exec : t -> bool
(** Is an executable? *)

val copy : src:t -> dst:t -> unit
(** Copy a file *)

val install :
  ?warning:OpamSystem.install_warning_fn ->
  ?exec:bool ->
  src:t ->
  dst:t ->
  unit ->
  unit
(** Installs a file to a destination. Optionally set if the destination should
    be set executable *)

val link : ?relative:bool -> target:t -> link:t -> unit
(** Symlink a file. If symlink is not possible on the system, use copy instead.
    With [relative], creates a relative link through the closest common ancestor
    directory if possible. Otherwise, the symlink is absolute. *)

val is_archive : t -> bool
(** Returns true if the given file is an archive (zip or tar) *)

val extract : t -> Dir.t -> unit
(** Extract an archive in a given directory (it rewrites the root to
    match [Dir.t] dir if needed) *)

val extract_job : t -> Dir.t -> exn option OpamProcess.job
(** Same as [extract], as an OpamProcess.job *)

val extract_in : t -> Dir.t -> unit
(** Extract an archive in a given directory *)

val extract_in_job : t -> Dir.t -> exn option OpamProcess.job

val make_tar_gz_job : t -> Dir.t -> exn option OpamProcess.job

val extract_generic_file : generic_file -> Dir.t -> unit
(** Extract a generic file *)

val starts_with : Dir.t -> t -> bool
(** Check whether a filename starts by a given Dir.t *)

val ends_with : string -> t -> bool
(** Check whether a filename ends with a given suffix *)

val dir_starts_with : Dir.t -> Dir.t -> bool
(** [dir starts_with pfx dir] Check whether [dir] starts with [pfx] *)

val dir_ends_with : string -> Dir.t -> bool
(** Check whether a dirname ends with a given suffix *)

val remove_prefix : Dir.t -> t -> string
(** Remove a prefix from a file name *)

val remove_prefix_dir : Dir.t -> Dir.t -> string

val remove_suffix : Base.t -> t -> string
(** Remove a suffix from a filename *)

val patch : ?preprocess:bool -> t -> Dir.t -> exn option OpamProcess.job
(** Apply a patch in a directory. If [preprocess] is set to false, there is no
    CRLF translation. Returns [None] on success, the process error otherwise *)

val touch : t -> unit
(** Create an empty file *)

val chmod : t -> int -> unit
(** Change file permissions *)

val find_in_parents : (Dir.t -> bool) -> Dir.t -> Dir.t option
(** Returns the closest parent of a directory (including itself) for which the
    predicate holds, if any *)

(** {2 Locking} *)

val flock : [< OpamSystem.lock_flag ] -> ?dontblock:bool -> t -> OpamSystem.lock
(** See [OpamSystem.flock]. Prefer the higher level [with_flock] functions when
    possible *)

val with_flock :
  [< OpamSystem.lock_flag ] ->
  ?dontblock:bool ->
  t ->
  (Unix.file_descr -> 'a) ->
  'a
(** Calls [f] while holding a lock file. Ensures the lock is properly released
    on [f] exit. Raises [OpamSystem.Locked] if [dontblock] is set and the lock
    can't be acquired. [f] is passed the file_descr of the lock. *)

val with_flock_upgrade :
  [< OpamSystem.actual_lock_flag ] ->
  ?dontblock:bool ->
  OpamSystem.lock ->
  (Unix.file_descr -> 'a) ->
  'a
(** Calls [f] with the file lock upgraded to at least [flag], then restores the
    previous lock level. Upgrade to [`Lock_write] should never be used in
    blocking mode as it would deadlock. Raises [OpamSystem.Locked] (but keeps
    the lock as is) if [dontblock] is set and the lock can't be upgraded. *)

val with_flock_write_then_read :
  ?dontblock:bool -> t -> (Unix.file_descr -> 'a) -> ('a -> 'b) -> 'b
(** Runs first function with a write lock on the given file, then releases it to
    a read lock and runs the second function. *)

module Op : sig
  val ( / ) : Dir.t -> string -> Dir.t
  (** Create a new directory *)

  val ( // ) : Dir.t -> string -> t
  (** Create a new filename *)
end

(** Simple structure to handle file attributes *)
module Attribute : sig
  include OpamStd.ABSTRACT

  val to_string_list : t -> string list

  val of_string_list : string list -> t

  val base : t -> Base.t
  (** Get remote filename *)

  val md5 : t -> OpamHash.t
  (** MD5 digest of the remote file *)

  val perm : t -> int option
  (** File permission *)

  val create : Base.t -> OpamHash.t -> int option -> t
  (** Constructor*)
end

val to_attribute : Dir.t -> t -> Attribute.t
(** Convert a filename to an attribute, relatively to a root *)
