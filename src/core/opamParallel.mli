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

(** Parallel execution of jobs following a directed graph *)

module type VERTEX = sig
  include OpamStd.OrderedType

  include Graph.Sig.COMPARABLE with type t := t
end

type dependency_label = unit

module type G = sig
  include Graph.Sig.I with type E.label = dependency_label

  module Vertex : VERTEX with type t = V.t

  module Topological : sig
    val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val has_cycle : t -> bool

  val scc_list : t -> V.t list list
end

exception Aborted
(** When one job fails due to an exception, other running jobs are interrupted
    and reported with this sub-exception in the Errors list *)

(** Simply parallel execution of tasks *)

exception Errors of int list * (int * exn) list * int list
(** In the simple iter, map and reduce cases, ints are the indexes of the jobs
    in the list *)

val iter :
  jobs:int ->
  command:('a -> unit OpamProcess.job) ->
  ?dry_run:bool ->
  'a list ->
  unit

val map :
  jobs:int ->
  command:('a -> 'b OpamProcess.job) ->
  ?dry_run:bool ->
  'a list ->
  'b list

val reduce :
  jobs:int ->
  command:('a -> 'b OpamProcess.job) ->
  merge:('b -> 'b -> 'b) ->
  nil:'b ->
  ?dry_run:bool ->
  'a list ->
  'b

(** More complex parallelism with dependency graphs *)

module type SIG = sig
  module G : G

  val iter :
    jobs:int ->
    command:(pred:(G.V.t * 'a) list -> G.V.t -> 'a OpamProcess.job) ->
    ?dry_run:bool ->
    ?pools:(G.V.t list * int) list ->
    G.t ->
    unit
  (** Runs the job [command ~pred v] for every node [v] in a graph, in
      topological order using [jobs] concurrent processes. Separate (possibly
      intersecting) node pools can be specified, with a separate number of
      allowed processes. The [jobs] maximum applies to the remaining nodes.

      The [pred] argument provided to the [command] function is the associative
      list of job results on direct predecessors of [v]. *)

  val map :
    jobs:int ->
    command:(pred:(G.V.t * 'a) list -> G.V.t -> 'a OpamProcess.job) ->
    ?dry_run:bool ->
    ?pools:(G.V.t list * int) list ->
    G.t ->
    (G.V.t * 'a) list
  (** Same as [iter], but returns the results of all jobs as a [vertex,result]
      associative list *)

  exception Errors of G.V.t list * (G.V.t * exn) list * G.V.t list
  (** Raised when the [command] functions raised exceptions. Parameters are
      (successfully traversed nodes, exception nodes and corresponding
      exceptions, remaining nodes that weren't traversed) *)

  exception Cyclic of G.V.t list list
  (** Raised when the graph to traverse has cycles. Returns the cycles found. *)
end

module Make (G : G) : SIG with module G = G and type G.V.t = G.V.t

module type GRAPH = sig
  include Graph.Sig.I with type E.label = dependency_label

  include Graph.Oper.S with type g = t

  module Topological : sig
    val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a

    val iter : (V.t -> unit) -> t -> unit
  end

  module Parallel : SIG with type G.t = t and type G.V.t = vertex

  module Dot : sig
    val output_graph : out_channel -> t -> unit
  end

  val transitive_closure : ?reflexive:bool -> t -> unit

  val build : V.t list -> E.t list -> t

  val compare : t -> t -> int

  val to_json : t OpamJson.encoder

  val of_json : t OpamJson.decoder
end

module MakeGraph (V : VERTEX) : GRAPH with type V.t = V.t
