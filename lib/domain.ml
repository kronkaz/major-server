module UserInfo = struct
  type t = {
    name : string
  }
end

module Election_summary = struct
  type t = {
    id : int;
    kind : string;
    name : string;
    date : string;
    is_running : bool;
  }

  let to_json { id; kind; name; date; is_running } = `Assoc [
    ("id", `Int id);
    ("kind", `String kind);
    ("name", `String name);
    ("date", `String date);
    ("is_running", `Bool is_running)
  ]
end

module Candidate = struct
  type t = {
    id : int;
    name : string;
    party : string;
    colour : string
  }
  let get_id { id; _ } = id

  let to_json { id; name; party; colour } = `Assoc [
    ("id", `Int id);
    ("name", `String name);
    ("party", `String party);
    ("colour", `String colour);
  ]

  let of_json_opt json = let (let*) = Option.bind in
    let* l = Utils.object_of_json_opt json in
    let* n = List.assoc_opt "name" l in
    let* p = List.assoc_opt "party" l in
    let* c = List.assoc_opt "colour" l in
    let* name = Utils.string_of_json_opt n in
    let* party = Utils.string_of_json_opt p in
    let* colour = Utils.string_of_json_opt c in
    Some { id = -1; name; party; colour }
end

module CandidateMap = Map.Make(struct
  type t = Candidate.t
  let compare candidate1 candidate2 = Candidate.(compare candidate1.id candidate2.id)
end)

module Election_info = struct
  type t = {
    id : int;
    kind : string;
    name : string;
    date : string;
    is_running : bool;
    candidates : Candidate.t list
  }

  let to_json { id; kind; name; date; is_running; candidates } = `Assoc [
    ("id", `Int id);
    ("kind", `String kind);
    ("name", `String name);
    ("date", `String date);
    ("is_running", `Bool is_running);
    ("candidates", `List (List.map Candidate.to_json candidates))
  ]
end

module Election = struct
  type t = {
    kind : string;
    name : string;
    date : string;
    candidates : Candidate.t list;
    voters : int list
  }

  let of_json_opt json = let (let*) = Option.bind in
    let* l = Utils.object_of_json_opt json in
    let* k = List.assoc_opt "kind" l in
    let* n = List.assoc_opt "name" l in
    let* d = List.assoc_opt "date" l in
    let* cs = List.assoc_opt "candidates" l in
    let* cs' = Utils.list_of_json_opt cs in
    let* vs = List.assoc_opt "voters" l in
    let* vs' = Utils.list_of_json_opt vs in
    let* kind = Utils.string_of_json_opt k in
    let* name = Utils.string_of_json_opt n in
    let* date = Utils.string_of_json_opt d in
    let* candidates = List.map Candidate.of_json_opt cs' |> Utils.option_sequence in
    let* voters = List.map Utils.int_of_json_opt vs' |> Utils.option_sequence in
    Some { kind; name; date; candidates; voters }
end

module Rating = struct
  type t = Rating of int

  let to_int (Rating i) = i
  let of_int_opt i = if i >= 0 && i < 7 then Some (Rating i) else None

  let max (Rating i) (Rating i') = Rating (max i i')
end
