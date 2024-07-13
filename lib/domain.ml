module Election_summary = struct
  type t = {
    id : int;
    name : string;
    is_running : bool;
  }

  let to_json { id; name; is_running } = `Assoc [
    ("id", `Int id);
    ("name", `String name);
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
end

module CandidateMap = Map.Make(struct
  type t = Candidate.t
  let compare candidate1 candidate2 = Candidate.(compare candidate1.id candidate2.id)
end)

module Election_info = struct
  type t = {
    id : int;
    name : string;
    is_running : bool;
    candidates : Candidate.t list
  }

  let to_json { id; name; is_running; candidates } = `Assoc [
    ("id", `Int id);
    ("name", `String name);
    ("is_running", `Bool is_running);
    ("candidates", `List (List.map Candidate.to_json candidates))
  ] 
end

module Rating = struct
  type t = Rating of int

  let to_int (Rating i) = i
  let of_int_opt i = if i >= 0 && i < 7 then Some (Rating i) else None

  let max (Rating i) (Rating i') = Rating (max i i')
end
