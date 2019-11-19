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

(** Bindings of lots of filesystem and system operations *)

exception Process_error of OpamProcess.result
(** Exception raised when subprocess fails *)

exception Command_not_found of string

val process_error : OpamProcess.result -> 'a
(** raise [Process_error] *)

val raise_on_process_error : OpamProcess.result -> unit
(** raise [Process_error] if the process didn't return 0 *)

exception Internal_error of string
(** Exception raised when a computation in the current process
    fails. *)

val internal_error : ('a, unit, string, 'b) format4 -> 'a
(** Raise [Internal_error] *)

val with_tmp_dir : (string -> 'a) -> 'a
(** [with_tmp_dir fn] executes [fn] in a tempory directory *)

val with_tmp_dir_job : (string -> 'a OpamProcess.job) -> 'a OpamProcess.job
(** Runs a job with a temp dir that is cleaned up afterwards *)

val verbose_for_base_commands : unit -> bool
(** Returns true if the default verbose level for base commands (cp, mv, etc.)
    is reached *)

val mk_temp_dir : ?prefix:string -> unit -> string
(** Returns a directory name, in the temporary directory, composed by {i opam}
    (if [prefix] is not set), pid, and random number. *)

val copy_file : string -> string -> unit
(** [copy_file src dst] copies [src] to [dst]. Remove [dst] before the copy
    if it is a link. *)

val copy_dir : string -> string -> unit
(** [copy_dir src dst] copies the contents of directory [src] into directory
    [dst], creating it if necessary, merging directory contents and ovewriting
    files otherwise *)

val mv : string -> string -> unit

type install_warning =
  [ `Add_exe (* [.exe] had to be added *)
  | `Install_dll (* Installation of [.dll] to bin/libexec *)
  | `Install_script (* Installation of script on Windows *)
  | `Install_unknown (* Installation of unknown file to bin/libexec *)
  | `Cygwin (* Installation of a Cygwin-linked executable *)
  | `Cygwin_libraries (* Installation of a binary linked to a Cygwin library *)
  ]
(** Warnings which come from {!install} *)

type install_warning_fn = string -> install_warning -> unit

val default_install_warning : install_warning_fn
(** The default warning function - displays a message on OpamConsole *)

val install :
  ?warning:install_warning_fn -> ?exec:bool -> string -> string -> unit
(** [install ?exec src dst] copies file [src] as file [dst] using [install].
    If [exec], make the resulting file executable (otherwise, look at the
    permissions of the original file to decide). *)

val is_exec : string -> bool
(** Checks if a file is an executable (regular file with execution
    permission) *)

val file_is_empty : string -> bool

val link : string -> string -> unit
(** [link src dst] links [src] to [dst]. Remove [dst] if it is a file,
    not a directory. *)

val real_path : string -> string
(** [real_path p] returns the real path associated to [p]: [..] are
    expanded and relative paths become absolute. *)

val string_of_channel : in_channel -> string
(** Return the contents of a channel. *)

exception File_not_found of string
(** Raised when a file or directory can't be accessed (doesn't exist,
    bad permissions, etc.) *)

val read : string -> string
(** [read filename] returns the contents of [filename] (while taking an advisory
    read lock to prevent concurrent writes) *)

val write : string -> string -> unit
(** [write filename contents] write [contents] to [filename] (while taking an
    advisory write lock to prevent concurrent reads or writes) *)

val remove : string -> unit
(** [remove filename] removes [filename]. Works whether [filename] is
    a file or a directory *)

val remove_file : string -> unit
(** [remove_file filename] removes [filename]. Works only for normal
    files (or also at least for symlinks) *)

val remove_dir : string -> unit
(** [remove_dir filename] removes [filename]. Works only for
    directory (not for symlinks or other files). *)

val chdir : string -> unit
(** Change the current working directory *)

val in_dir : string -> (unit -> 'a) -> 'a
(** [in_dir dir fn] evaluates [fn] in the directory [dir] *)

val ls : string -> string list
(** Returns the list of files and directories in the given directory (full
    names) *)

val files_with_links : string -> string list
(** [files_with_links dir] returns the files in the directory [dir].
    Links simulating directory are ignored, others links are returned. *)

val rec_files : string -> string list
(** [rec_files dir] returns the list of all files in [dir],
    recursively.
    Links behaving like directory are crossed. *)

val files : string -> string list
(** Return the list of files in the current directory. *)

val rec_dirs : string -> string list
(** [rec_dirs dir] return the list list of all directories recursively
    (going through symbolink links). *)

val dirs : string -> string list
(** Return the list of directories in the current directory. *)

val dir_is_empty : string -> bool

val directories_with_links : string -> string list
(** [directories_with_links dir] returns the directories in the directory [dir].
    Links pointing to directory are also returned. *)

val make_command :
  ?verbose:bool ->
  ?env:string array ->
  ?name:string ->
  ?text:string ->
  ?metadata:(string * string) list ->
  ?allow_stdin:bool ->
  ?stdout:string ->
  ?dir:string ->
  ?resolve_path:bool ->
  string ->
  string list ->
  OpamProcess.command
(** Make a comman suitable for OpamProcess.Job. if [verbose], is set,
    command and output will be displayed (at command end for the
    latter, if concurrent commands are running). [name] is used for
    naming log files. [text] is what is displayed in the status line
    for this command. May raise Command_not_found, unless
    [resolve_path] is set to false (in which case you can end up
    with a process error instead) *)

(** OLD COMMAND API, DEPRECATED *)

type command = string list
(** a command is a list of words *)

val resolve_command :
  ?env:string array -> ?dir:string -> string -> string option
(** Test whether a command exists in the environment, and returns it (resolved
    if found in PATH) *)

val get_cygpath_function : command:string -> (string -> string) lazy_t
(** Returns a function which should be applied to arguments for a given command
    by determining if the command is the Cygwin variant of the command. Returns
    the identity function otherwise. *)

val command :
  ?verbose:bool ->
  ?env:string array ->
  ?name:string ->
  ?metadata:(string * string) list ->
  ?allow_stdin:bool ->
  command ->
  unit
(** [command cmd] executes the command [cmd] in the correct OPAM
    environment. *)

val commands :
  ?verbose:bool ->
  ?env:string array ->
  ?name:string ->
  ?metadata:(string * string) list ->
  ?keep_going:bool ->
  command list ->
  unit
(** [commands cmds] executes the commands [cmds] in the correct OPAM
    environment. It stops whenever one command fails unless [keep_going] is set
    to [true]. In this case, the first error is re-raised at the end. *)

val read_command_output :
  ?verbose:bool ->
  ?env:string array ->
  ?metadata:(string * string) list ->
  ?allow_stdin:bool ->
  command ->
  string list
(** [read_command_output cmd] executes the command [cmd] in the
    correct OPAM environment and return the lines from stdout if the command
    exists normally. If the command does not exist or if the command exited
    with a non-empty exit-code, throw an error. *)

(** END *)

val is_archive : string -> bool
(** Test whether the file is an archive, by looking as its extension *)

val extract : dir:string -> string -> unit
(** [extract ~dir:dirname filename] extracts the archive [filename] into
    [dirname]. [dirname] should not exists and [filename] should
    contain only one top-level directory.*)

val extract_job : dir:string -> string -> exn option OpamProcess.job
(** Same as [extract], but as an OpamProcess.job *)

val extract_in : dir:string -> string -> unit
(** [extract_in ~dir:dirname filename] extracts the archive [filename] into
    [dirname]. *)

val extract_in_job : dir:string -> string -> exn option OpamProcess.job
(** [extract_in_job] is similar to [extract_in], but as a job *)

val make_tar_gz_job : dir:string -> string -> exn option OpamProcess.job

val mkdir : string -> unit
(** Create a directory. Do not fail if the directory already
    exist. *)

val cpu_count : unit -> int
(** Get the number of active processors on the system *)

(** {2 File locking function} *)

type lock
(** Unix file locks (mutable structure, to follow actual semantics) *)

type actual_lock_flag = [ `Lock_read | `Lock_write ]
(** The different kinds of unix advisory locks available (`Lock_none doesn't
    actually lock anything, or even create the lock file) *)

type lock_flag = [ `Lock_none | actual_lock_flag ]

val lock_none : lock
(** Dummy lock *)

exception Locked
(** Raised when locks can't be acquired and [dontblock] was specified) *)

val release_all_locks : unit -> unit
(** Force releases all open locks in the process. Required for Windows if an exception
    has been raised, since Windows doesn't permit unlinking while handles are open. *)

val flock : [< lock_flag ] -> ?dontblock:bool -> string -> lock
(** Acquires a lock on the given file.
    Raises [Locked] if the lock can't be acquired and [dontblock] is set. Raises
    [OpamStd.Sys.Exit] if [safe_mode] is set and a write lock is required. Also
    raises Unix errors if the lock file can't be opened. *)

val flock_update : [< lock_flag ] -> ?dontblock:bool -> lock -> unit
(** Updates an existing lock to the given level. Raises the same exceptions as
    [flock]. *)

val funlock : lock -> unit
(** Releases an acquired lock (equivalent to [flock_update `Lock_none]) *)

val lock_max : lock_flag -> lock_flag -> lock_flag
(** Returns the highest of the two lock flags (with the order no lock < read
    lock < write lock) *)

val lock_isatleast : [< lock_flag ] -> lock -> bool
(** Returns true if the lock already has the lock_flag rights or more *)

val get_lock_flag : lock -> lock_flag
(** Returns the current kind of the lock *)

val get_lock_fd : lock -> Unix.file_descr
(** Returns the underlying fd for the lock or raises Not_found for `No_lock *)

(** {2 Misc} *)

val patch :
  ?preprocess:bool -> dir:string -> string -> exn option OpamProcess.job
(** Apply a patch file in the current directory. If [preprocess] is set to
    false, there is no CRLF translation. Returns the error if the patch didn't
    apply. *)

val get_eol_encoding : string -> bool option
(** Returns the end-of-line encoding style for the given file. [None] means that
    either the encoding of line endings is mixed, or the file contains no line
    endings at all (an empty file, or a file with one line and no EOL at EOF).
    Otherwise it returns [Some true] if all endings are encoded CRLF. *)

val translate_patch : dir:string -> string -> string -> unit
(** [translate_patch ~dir input_patch output_patch] writes a copy of
    [input_patch] to [output_patch] as though [input_patch] had been applied in
    [dir]. The patch is rewritten such that if text files have different line
    endings then the patch is transformed to patch using the encoding on disk.
    In particular, this means that patches generated against Unix checkouts of
    Git sources will correctly apply to Windows checkouts of the same sources.
*)

val temp_file : ?auto_clean:bool -> ?dir:string -> string -> string
(** Create a temporary file in {i ~/.opam/logs/<name>XXX}, if [dir] is not set.
    ?auto_clean controls whether the file is automatically deleted when opam
    terminates (default: [true]). *)

val print_stats : unit -> unit
(** Print stats *)

val register_printer : unit -> unit
(** Registers an exception printer that adds some OPAM version info, and details
    on process and Unix errors *)

val init : unit -> unit
(** Initialises signal handlers, catch_break and some exception printers. The
    lib may not perform properly without this if [Sys.catch_break] isn't set
    and SIGPIPE isn't handled (with a no-op) *)

val forward_to_back : string -> string
(** On Unix, a no-op. On Windows, convert / to \ *)

val back_to_forward : string -> string
(** On Unix, a no-op. On Windows, convert \ to / *)

val classify_executable :
  string ->
  [ `Exe of [ `i386 | `x86 | `x86_64 ]
  | `Dll of [ `x86 | `x86_64 ]
  | `Script
  | `Unknown ]
(** Identifies kinds of executable files. At present, only useful on Windows.
    Executable or DLLs are recognised based on their content, not on their
    filename. Any file beginning "#!" is assumed to be a shell script and all
    files are classified [`Unknown]. *)
