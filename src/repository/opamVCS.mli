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

(** Layer for handling version control sources through a functor *)

open OpamTypes

(** Each backend should implement this signature. *)
module type VCS = sig
  val name : OpamUrl.backend

  val exists : dirname -> bool
  (** Test whether the given repository is correctly initialized. *)

  val init : dirname -> url -> unit OpamProcess.job
  (** Init a repository. *)

  val fetch : ?cache_dir:dirname -> dirname -> url -> unit OpamProcess.job
  (** Fetch changes from upstream. This is supposed to put the changes
      in a staging area.
      Be aware that the remote URL might have been changed, so make sure
      to update accordingly. *)

  val reset_tree : dirname -> url -> unit OpamProcess.job
  (** Reset the master branch of the repository to match the remote repository
      state. This might still fetch more data (git submodules...), so is
      unsuitable for running after validation. *)

  val patch_applied : dirname -> url -> unit OpamProcess.job
  (** Confirm that applying the patch results in a clean synchronization of
      the working tree with its repository state. *)

  val diff : dirname -> url -> filename option OpamProcess.job
  (** Returns the pending modifications in the form of a patch file, or None if
      [dirname] is up to date with what was last fetched. *)

  val is_up_to_date : dirname -> url -> bool OpamProcess.job
  (** Returns true if the last fetched state is equal to the current, on-disk
      state *)

  val revision : dirname -> string option OpamProcess.job
  (** Returns an backend-specific identifier for the current revision. *)

  val versioned_files : dirname -> string list OpamProcess.job
  (** Returns the list of files under version control *)

  val vc_dir : dirname -> dirname
  (** Returns the absolute directory name for vc data (e.g.
      [.../project/.git]) *)

  val current_branch : dirname -> string option OpamProcess.job
  (** Returns the currently selected branch handle. It should be valid as the
      [hash] field of [OpamUrl.t]. *)

  val is_dirty : dirname -> bool OpamProcess.job
  (** Returns true if the working tree state is different from the state
      recorded in the VCS as current. This differs from [is_up_to_date], which
      compares specifically to the last fetched state. This should always be
      [false] after [reset] has been called. *)

  val modified_files : dirname -> string list OpamProcess.job
  (** Returns the list of files under version control, modified in the working
      tree but not comitted *)

  val get_remote_url : ?hash:string -> dirname -> url option OpamProcess.job
end

(** Create a backend from a [VCS] implementation. *)
module Make (VCS : VCS) : OpamRepositoryBackend.S
