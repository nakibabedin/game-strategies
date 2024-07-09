open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    let n = Game.Game_kind.board_length game.game_kind in
    let rows = List.init n ~f:(fun row -> row) in
    let cols = List.init n ~f:(fun col -> col) in
    List.iter rows ~f:(fun row ->

      (* Print out the row *)
      List.iter cols ~f:(fun column ->
        (match Map.find game.board { row; column } with
         | Some piece -> printf "%s " (Game.Piece.to_string piece)
         | None -> printf "  ");
        if Int.(column <> n - 1) then printf "|";);
        
        (* Print out the line below *)
        if Int.(row <> n - 1)
           (* not (equal row (Game.Game_kind.board_length game.game_kind -
              1)) *)
        then (
          printf "\n";
          let _ = List.init (Int.pow n 2) ~f:(fun _ -> printf "-") in
          printf "\n"))
  ;;

  (* let print_game (game : Game.t) =
    (* let occupied_positions_list = Map.keys game.board in *)
    (* String.concat *)
    let n = Game.Game_kind.board_length game.game_kind in
    let _ =
      List.init n ~f:(fun r ->
        let row = n - r in
        let _ =
          List.init n ~f:(fun c ->
            let column = n - c in
            (* printf "row: %d col: %d" row column *)
            (match Map.find game.board { row; column } with
             | Some piece -> printf "%s" (Game.Piece.to_string piece)
             | None -> printf " ");
            if Int.(column <> n - 1) then printf " | ")
        in
        if Int.(row <> n - 1)
           (* not (equal row (Game.Game_kind.board_length game.game_kind -
              1)) *)
        then (
          printf "\n";
          let _ = List.init (Int.pow n 2) ~f:(fun _ -> printf "-") in
          printf "\n"))
    in
    ()
  ;; *)

  (* ignore game; *)
  (* print_endline "" *)

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X |O |X
      ---------
      O |O |X
      ---------
      O |X |X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |  |
      ---------
      O |  |
      ---------
      O |  |X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ]
  ;;
end

(* module Echo = struct module Query = struct type t = string [@@deriving
   sexp_of, bin_io] end

   module Response = struct type t = { time : Time_ns_unix.t ; message:
   String.t } [@@deriving sexp_of, bin_io] end end

   module Rpcs = struct let echo_rpc = Rpc.Rpc.create ~name:"echo ping"
   ~version:0 ~bin_query:Echo.Query.bin_t ~bin_response:Echo.Response.bin_t
   ;; end *)

let handle_rpc (_client : unit) (_query : Rpcs.Take_turn.Query.t) =
  let response =
    { Rpcs.Take_turn.Response.piece = Game.Piece.of_string "X"
    ; Rpcs.Take_turn.Response.position = { row = 0; column = 0 }
    }
  in
  return response
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let%bind server =
         let implementations =
           Rpc.Implementations.create_exn
             ~on_unknown_rpc:`Close_connection
             ~implementations:
               [ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_rpc ]
         in
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
