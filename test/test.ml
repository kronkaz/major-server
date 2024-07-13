open OUnit2
open Major
open Domain

(*   ┌───────────┐                                                                                *)
(* ──┤ Test data ├─────────────────────────────────────────────────────────────────────────────── *)
(*   └───────────┘                                                                                *)

(* easy conversion of percentage data to integers,
   by supposing there are 100, 1000, 10000 voters, etc.
     — the result is the same anyway *)

let candidate_wikiFRA = Candidate.{ id = 1; name = "wikiFRA"; party = ""; colour = "" }
let candidate_wikiFRB = Candidate.{ id = 2; name = "wikiFRB"; party = ""; colour = "" }
let candidate_wikiFRC = Candidate.{ id = 3; name = "wikiFRC"; party = ""; colour = "" }
let votes_wikiFRA = [|1484; 1763;  912; 1971; 2128; 1742; 0|]
let votes_wikiFRB = [|2427; 1158; 1342; 1295; 2073; 1705; 0|]
let votes_wikiFRC = [|2500; 2500; 1500; 1500; 1000; 1000; 0|]

let candidate_wikiFRE = Candidate.{ id = 4; name = "wikiFRE"; party = ""; colour = "" }
let candidate_wikiFRF = Candidate.{ id = 5; name = "wikiFRF"; party = ""; colour = "" }
let votes_wikiFRE = [|0; 1; 2; 2; 0; 0; 0|]
let votes_wikiFRF = [|0; 0; 3; 2; 0; 0; 0|]
let votes_wikiFRE2 = [|1; 0; 2; 2; 1; 0; 0|]
let votes_wikiFRF2 = [|0; 1; 2; 3; 0; 0; 0|]

let aubry = Candidate.{ id = 9; name = "MA"; party = ""; colour = "" }
let borloo = Candidate.{ id = 4; name = "JLB"; party = ""; colour = "" }
let villepin = Candidate.{ id = 1; name = "DDV"; party = ""; colour = "" }
let bayrou = Candidate.{ id = 0; name = "FB"; party = ""; colour = "" }
let joly = Candidate.{ id = 8; name = "EJ"; party = ""; colour = "" }
let sarkozy = Candidate.{ id = 2; name = "NS"; party = ""; colour = "" }
let chevenement = Candidate.{ id = 5; name = "JPC"; party = ""; colour = "" }
let melenchon = Candidate.{ id = 3; name = "Jonluk"; party = ""; colour = "" }
let besancenot = Candidate.{ id = 7; name = "Besancenot Limit"; party = ""; colour = "" }
let dupont_aignan = Candidate.{ id = 11; name = "NDA"; party = ""; colour = "" }
let arthaud = Candidate.{ id = 10; name = "NA"; party = ""; colour = "" }
let le_pen = Candidate.{ id = 6; name = "MLP"; party = ""; colour = "" }
let votes_aubry2011 =         [|184; 114; 196; 126; 170; 129; 82|]
let votes_borloo2011 =        [|185; 159; 196; 223; 153;  62; 22|]
let votes_villepin2011 =      [|219; 174; 207; 204; 119;  58; 20|]
let votes_bayrou2011 =        [|193; 166; 261; 192; 128;  47; 12|]
let votes_joly2011 =          [|309; 190; 203; 145;  74;  47; 32|]
let votes_sarkozy2011 =       [|413; 118; 135;  95; 111;  87; 41|]
let votes_chevenement2011 =   [|322; 247; 228; 129;  58;  11;  5|]
let votes_melenchon2011 =     [|418; 214; 165; 112;  50;  27; 13|]
let votes_besancenot2011 =    [|442; 204; 161;  99;  69;  17;  8|]
let votes_dupont_aignan2011 = [|467; 277; 139;  70;  27;  14;  5|]
let votes_arthaud2011 =       [|480; 261; 137;  77;  34;   9;  1|]
let votes_le_pen2011 =        [|556;  93;  78;  72;  70;  65; 68|]

let pecresse = Candidate.{ id = 29; name = "VP"; party = ""; colour = "" }
let macron = Candidate.{ id = 21; name = "EM"; party = ""; colour = "" }
let montebourg = Candidate.{ id = 22; name = "AM"; party = ""; colour = "" }
let jadot = Candidate.{ id = 20; name = "YJ"; party = ""; colour = "" }
let hidalgo = Candidate.{ id = 25; name = "AH"; party = ""; colour = "" }
let roussel = Candidate.{ id = 23; name = "so6"; party = ""; colour = "" }
let taubira = Candidate.{ id = 24; name = "CT"; party = ""; colour = "" }
let poutou = Candidate.{ id = 27; name = "P2"; party = ""; colour = "" }
let zemmour = Candidate.{ id = 26; name = "Z"; party = ""; colour = "" }

let votes_pecresse2021 =      [|24; 13; 17; 17; 15; 10; 4|]
let votes_macron2021 =        [|32; 11; 14; 10; 14; 12; 7|]
let votes_montebourg2021 =    [|33; 26; 20; 11;  7;  2; 1|]
let votes_jadot2021 =         [|36; 24; 17; 11;  7;  4; 1|]
let votes_hidalgo2021 =       [|43; 20; 17;  9;  7;  3; 1|]
let votes_roussel2021 =       [|43; 28; 16;  7;  3;  2; 1|]
let votes_dupont_aignan2021 = [|45; 20; 15;  9;  6;  3; 2|]
let votes_le_pen2021 =        [|47;  9; 10;  8;  9;  9; 8|]
let votes_taubira2021 =       [|48; 17; 12; 10;  7;  3; 3|]
let votes_arthaud2021 =       [|49; 24; 16;  7;  3;  1; 0|]
let votes_poutou2021 =        [|52; 23; 12;  6;  4;  2; 1|]
let votes_melenchon2021 =     [|53; 14; 12;  7;  7;  3; 4|]
let votes_zemmour2021 =       [|61;  7;  7;  6;  6;  7; 6|]

(*   ┌──────────────┐                                                                             *)
(* ──┤ Test helpers ├──────────────────────────────────────────────────────────────────────────── *)
(*   └──────────────┘                                                                             *)

let test_majority_rating name candidate_votes ~expected = name >:: fun _ ->
  assert_equal expected (Judgment.majority_rating ~candidate_votes)

let test_majority_sequence name candidate_votes ~expected = name >:: fun _ ->
  assert_equal
    (expected
     |> List.map @@ fun i -> Option.get @@ Rating.of_int_opt i)
    (Judgment.majority_sequence ~candidate_votes
     |> List.of_seq)

let test_majority_judgment name votes ~expected_ratings ~expected_winners = name >:: fun _ ->
  let majority_ratings, winners = Judgment.majority_judgment ~votes in
  assert_equal
    (expected_ratings
     |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2))
    (majority_ratings
     |> CandidateMap.to_list
     |> List.map (fun (Candidate.{ id; _ }, rating) -> id, rating));
  assert_equal
    (expected_winners |> List.sort Int.compare)
    (winners          |> List.map @@ fun Candidate.{ id; _ } -> id)

(*   ┌─────────────┐                                                                              *)
(* ──┤ Test suites ├───────────────────────────────────────────────────────────────────────────── *)
(*   └─────────────┘                                                                              *)

let test_suite_majority_rating = "test for Judgment.majority_rating" >::: [
  test_majority_rating "no_vote" [|0; 0; 0; 0; 0; 0; 0|] ~expected:None;
  (* French Wikipedia, first example *)
  test_majority_rating "wikiFRA" votes_wikiFRA ~expected:(Rating.of_int_opt 3);
  test_majority_rating "wikiFRB" votes_wikiFRB ~expected:(Rating.of_int_opt 3);
  test_majority_rating "wikiFRC" votes_wikiFRC ~expected:(Rating.of_int_opt 1);
  (* French Wikipedia, second example *)
  test_majority_rating "wikiFRE" votes_wikiFRE ~expected:(Rating.of_int_opt 2);
  test_majority_rating "wikiFRF" votes_wikiFRF ~expected:(Rating.of_int_opt 2);
  (* French Wikipedia, third example *)
  test_majority_rating "wikiFRE2" votes_wikiFRE2 ~expected:(Rating.of_int_opt 2);
  test_majority_rating "wikiFRF2" votes_wikiFRF2 ~expected:(Rating.of_int_opt 2);
  (* French Wikipedia, real election examples *)
  test_majority_rating "aubry2011"         votes_aubry2011         ~expected:(Rating.of_int_opt 3);
  test_majority_rating "borloo2011"        votes_borloo2011        ~expected:(Rating.of_int_opt 2);
  test_majority_rating "villepin2011"      votes_villepin2011      ~expected:(Rating.of_int_opt 2);
  test_majority_rating "bayrou2011"        votes_bayrou2011        ~expected:(Rating.of_int_opt 2);
  test_majority_rating "joly2011"          votes_joly2011          ~expected:(Rating.of_int_opt 2);
  test_majority_rating "sarkozy2011"       votes_sarkozy2011       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "chevenement2011"   votes_chevenement2011   ~expected:(Rating.of_int_opt 1);
  test_majority_rating "melenchon2011"     votes_melenchon2011     ~expected:(Rating.of_int_opt 1);
  test_majority_rating "besancenot2011"    votes_besancenot2011    ~expected:(Rating.of_int_opt 1);
  test_majority_rating "dupont_aignan2011" votes_dupont_aignan2011 ~expected:(Rating.of_int_opt 1);
  test_majority_rating "arthaud2011"       votes_arthaud2011       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "le_pen2011"        votes_le_pen2011        ~expected:(Rating.of_int_opt 0);

  test_majority_rating "pecresse2021"      votes_pecresse2021      ~expected:(Rating.of_int_opt 2);
  test_majority_rating "macron2021"        votes_macron2021        ~expected:(Rating.of_int_opt 2);
  test_majority_rating "montebourg2021"    votes_montebourg2021    ~expected:(Rating.of_int_opt 1);
  test_majority_rating "jadot2021"         votes_jadot2021         ~expected:(Rating.of_int_opt 1);
  test_majority_rating "hidalgo2021"       votes_hidalgo2021       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "roussel2021"       votes_roussel2021       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "dupont_aignan2021" votes_dupont_aignan2021 ~expected:(Rating.of_int_opt 1);
  test_majority_rating "le_pen2021"        votes_le_pen2021        ~expected:(Rating.of_int_opt 1);
  test_majority_rating "taubira2021"       votes_taubira2021       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "arthaud2021"       votes_arthaud2021       ~expected:(Rating.of_int_opt 1);
  test_majority_rating "poutou2021"        votes_poutou2021        ~expected:(Rating.of_int_opt 0);
  test_majority_rating "melenchon2021"     votes_melenchon2021     ~expected:(Rating.of_int_opt 0);
  test_majority_rating "zemmour2021"       votes_zemmour2021       ~expected:(Rating.of_int_opt 0);
]

(* for this one, testing real vote data is impossible because the sequence is too long *)
let test_suite_majority_sequence = "test for Judgment.majority_sequence" >::: [
  test_majority_sequence "no_vote" [|0; 0; 0; 0; 0; 0; 0|] ~expected:[];
  test_majority_sequence "wikiFRE" votes_wikiFRE ~expected:[2; 2; 3; 1; 3];
  test_majority_sequence "wikiFRF" votes_wikiFRF ~expected:[2; 2; 3; 2; 3];
  test_majority_sequence "wikiFRE2" votes_wikiFRE2 ~expected:[2; 3; 2; 3; 0; 4];
  test_majority_sequence "wikiFRF2" votes_wikiFRF2 ~expected:[2; 3; 2; 3; 1; 3];
]

let test_suite_majority_judgment = "test for Judgment.majority_judgment" >::: [
  test_majority_judgment "no_vote1"
    (CandidateMap.of_list [
      (candidate_wikiFRA, [|0; 0; 0; 0; 0; 0; 0|])
    ])
    ~expected_ratings:[
      (candidate_wikiFRA.id, None)
    ]
    ~expected_winners:[candidate_wikiFRA.id];
  test_majority_judgment "no_vote3"
    (CandidateMap.of_list [
      (candidate_wikiFRA, [|0; 0; 0; 0; 0; 0; 0|]);
      (candidate_wikiFRB, [|0; 0; 0; 0; 0; 0; 0|]);
      (candidate_wikiFRC, [|0; 0; 0; 0; 0; 0; 0|]);
    ])
    ~expected_ratings:[
      (candidate_wikiFRA.id, None);
      (candidate_wikiFRB.id, None);
      (candidate_wikiFRC.id, None);
    ]
    ~expected_winners:[candidate_wikiFRA.id; candidate_wikiFRB.id; candidate_wikiFRC.id];
  test_majority_judgment "wikiFRABC"
    (CandidateMap.of_list [
      (candidate_wikiFRA, votes_wikiFRA);
      (candidate_wikiFRB, votes_wikiFRB);
      (candidate_wikiFRC, votes_wikiFRC);
    ])
    ~expected_ratings:[
      (candidate_wikiFRA.id, Rating.of_int_opt 3);
      (candidate_wikiFRB.id, Rating.of_int_opt 3);
      (candidate_wikiFRC.id, Rating.of_int_opt 1);
    ]
    ~expected_winners:[candidate_wikiFRA.id];
  test_majority_judgment "wikiFREF"
    (CandidateMap.of_list [
      (candidate_wikiFRE, votes_wikiFRE);
      (candidate_wikiFRF, votes_wikiFRF);
    ])
    ~expected_ratings:[
      (candidate_wikiFRE.id, Rating.of_int_opt 2);
      (candidate_wikiFRF.id, Rating.of_int_opt 2);
    ]
    ~expected_winners:[candidate_wikiFRF.id];
  test_majority_judgment "wikiFREF2"
    (CandidateMap.of_list [
      (candidate_wikiFRE, votes_wikiFRE2);
      (candidate_wikiFRF, votes_wikiFRF2);
    ])
    ~expected_ratings:[
      (candidate_wikiFRE.id, Rating.of_int_opt 2);
      (candidate_wikiFRF.id, Rating.of_int_opt 2);
    ]
    ~expected_winners:[candidate_wikiFRF.id];
  test_majority_judgment "2011"
    (CandidateMap.of_list [
      (aubry,         votes_aubry2011);
      (borloo,        votes_borloo2011);
      (villepin,      votes_villepin2011);
      (bayrou,        votes_bayrou2011);
      (joly,          votes_joly2011);
      (sarkozy,       votes_sarkozy2011);
      (chevenement,   votes_chevenement2011);
      (melenchon,     votes_melenchon2011);
      (besancenot,    votes_besancenot2011);
      (dupont_aignan, votes_dupont_aignan2011);
      (arthaud,       votes_arthaud2011);
      (le_pen,        votes_le_pen2011)
    ])
    ~expected_ratings:[
      (aubry.id,         Rating.of_int_opt 3);
      (borloo.id,        Rating.of_int_opt 2);
      (villepin.id,      Rating.of_int_opt 2);
      (bayrou.id,        Rating.of_int_opt 2);
      (joly.id,          Rating.of_int_opt 2);
      (sarkozy.id,       Rating.of_int_opt 1);
      (chevenement.id,   Rating.of_int_opt 1);
      (melenchon.id,     Rating.of_int_opt 1);
      (besancenot.id,    Rating.of_int_opt 1);
      (dupont_aignan.id, Rating.of_int_opt 1);
      (arthaud.id,       Rating.of_int_opt 1);
      (le_pen.id,        Rating.of_int_opt 0)
    ]
    ~expected_winners:[aubry.id];
  test_majority_judgment "2021"
    (CandidateMap.of_list [
      (pecresse,      votes_pecresse2021);
      (macron,        votes_macron2021);
      (montebourg,    votes_montebourg2021);
      (jadot,         votes_jadot2021);
      (hidalgo,       votes_hidalgo2021);
      (roussel,       votes_roussel2021);
      (dupont_aignan, votes_dupont_aignan2021);
      (le_pen,        votes_le_pen2021);
      (taubira,       votes_taubira2021);
      (arthaud,       votes_arthaud2021);
      (poutou,        votes_poutou2021);
      (melenchon,     votes_melenchon2021);
      (zemmour,       votes_zemmour2021);
    ])
    ~expected_ratings:[
      (pecresse.id,      Rating.of_int_opt 2);
      (macron.id,        Rating.of_int_opt 2);
      (montebourg.id,    Rating.of_int_opt 1);
      (jadot.id,         Rating.of_int_opt 1);
      (hidalgo.id,       Rating.of_int_opt 1);
      (roussel.id,       Rating.of_int_opt 1);
      (dupont_aignan.id, Rating.of_int_opt 1);
      (le_pen.id,        Rating.of_int_opt 1);
      (taubira.id,       Rating.of_int_opt 1);
      (arthaud.id,       Rating.of_int_opt 1);
      (poutou.id,        Rating.of_int_opt 0);
      (melenchon.id,     Rating.of_int_opt 0);
      (zemmour.id,       Rating.of_int_opt 0);
    ]
    ~expected_winners:[pecresse.id];
]

let () = List.iter run_test_tt_main [
  test_suite_majority_rating;
  test_suite_majority_sequence;
  test_suite_majority_judgment
]

(* let () =
  let module App = App.Make(struct
    module Auth = Auth_service.Default
    module Db = Database_service.Default
  end) in
  App.start () *)