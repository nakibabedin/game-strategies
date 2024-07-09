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

  let close_to_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let _empty_board = empty_game

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
        if Int.(column <> n - 1) then printf "| ");
      (* Print out the line below *)
      if Int.(row <> n - 1)
         (* not (equal row (Game.Game_kind.board_length game.game_kind -
            1)) *)
      then (
        printf "\n";
        let _ = List.init (Int.pow n 2) ~f:(fun _ -> printf "-") in
        printf "\n"))
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  (* let%expect_test "print_non_win" = print_game non_win; [%expect {| X | |
     --------- | | --------- | | |}]; return () *)

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let n = Game.Game_kind.board_length game.game_kind in
    let rows = List.init n ~f:(fun row -> row) in
    let all_positions =
      List.concat_map rows ~f:(fun row ->
        List.init n ~f:(fun col -> { Game.Position.row; column = col }))
    in
    let occupied_positions = Map.keys game.board in
    List.filter all_positions ~f:(fun pos ->
      not (List.exists occupied_positions ~f:(Game.Position.equal pos)))
  ;;

  let get_info_for_evaluate ~game =
    let open Game in
    (* Win condition represents number of things in a row needed to win *)
    let win_condition_length =
      match game.game_kind with
      | Game_kind.Tic_tac_toe -> 3
      | Game_kind.Omok -> 5
    in
    game.board, win_condition_length
  ;;

  let check_row ~game ~(position : Game.Position.t) =
    let game_board, win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let entire_row_opt =
      List.init win_condition_length ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row; column = check_col + length })
    in
    let entire_row = List.filter_opt entire_row_opt in
    match List.all_equal entire_row ~equal:Game.Piece.equal with
    | Some _ -> equal (List.length entire_row) win_condition_length
    | None -> false
  ;;

  let check_col ~game ~(position : Game.Position.t) =
    let game_board, win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let entire_col_opt =
      List.init win_condition_length ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row + length; column = check_col })
    in
    let entire_col = List.filter_opt entire_col_opt in
    match List.all_equal entire_col ~equal:Game.Piece.equal with
    | Some _ -> equal (List.length entire_col) win_condition_length
    | None -> false
  ;;

  let check_diagonal ~game ~(position : Game.Position.t) =
    let game_board, win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let forward_diagonal_opt =
      List.init win_condition_length ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row + length
          ; column = check_col + length
          })
    in
    let backward_diagnoal_opt =
      List.init win_condition_length ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row + length
          ; column = check_col - length
          })
    in
    let forward_diagonal = List.filter_opt forward_diagonal_opt in
    let backward_diagnoal = List.filter_opt backward_diagnoal_opt in
    let win_via_forward_diagonal =
      match List.all_equal forward_diagonal ~equal:Game.Piece.equal with
      | Some _ -> equal (List.length forward_diagonal) win_condition_length
      | None -> false
    in
    let win_via_backward_diagonal =
      match List.all_equal backward_diagnoal ~equal:Game.Piece.equal with
      | Some _ -> equal (List.length backward_diagnoal) win_condition_length
      | None -> false
    in
    win_via_backward_diagonal || win_via_forward_diagonal
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let occupied_positions = Map.keys game.board in
    let winning_position =
      List.filter occupied_positions ~f:(fun pos ->
        check_col ~game ~position:pos
        || check_row ~game ~position:pos
        || check_diagonal ~game ~position:pos)
    in
    match winning_position with
    | [] -> Game_continues
    | h :: t ->
      (match t with
       | [] -> Game_over { winner = Map.find game.board h }
       | _ :: _ -> Illegal_move)
  ;;

  let check_move_for_win ~game ~piece ~position =
    let new_game = place_piece game ~piece ~position in
    let move_outcome = evaluate new_game in
    match move_outcome with
    | Game_over outcome ->
      (match outcome.winner with
       | Some winning_piece -> Game.Piece.equal winning_piece piece
       | None -> false)
    | Illegal_move | Game_continues -> false
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let possible_next_moves = available_moves game in
    List.filter possible_next_moves ~f:(fun position ->
      check_move_for_win ~game ~piece:me ~position)
  ;;

  let check_move_for_loss ~game ~piece ~position =
    let new_game = place_piece game ~piece ~position in
    let possible_losses =
      match piece with
      | X -> winning_moves new_game ~me:O
      | O -> winning_moves new_game ~me:X
    in
    List.length possible_losses > 0
  ;;

  (* Game.Evaluation.t = let occupied_positions = Map.keys game.board in

     (* Calculate the number of items in a row *) let number_in_a_row = () in
     let number_in_a_col = () in let number_in_a_diagonal = () in *)

  (* Exercise 4 *)
  let moves_where_my_opponent_wins ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let possible_next_moves = available_moves game in
    List.filter possible_next_moves ~f:(fun position ->
      check_move_for_loss ~game ~piece:me ~position)
  ;;

  (* Exercise 5 *)
  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let open Game in
    let possible_next_moves = available_moves game in
    let moves_that_lose = moves_where_my_opponent_wins ~me game in
    List.filter possible_next_moves ~f:(fun pos ->
      not (List.exists moves_that_lose ~f:(Position.equal pos)))
  ;;

  let%expect_test "exercise_five" =
    let non_losing_moves =
      available_moves_that_do_not_immediately_lose ~me:X close_to_win
    in
    print_s [%sexp (non_losing_moves : Game.Position.t list)];
    [%expect {| () |}];
    return ()
  ;;

  let heuristic_function ~(game : Game.t) ~(piece : Game.Piece.t) =
    let opponent =
      match piece with X -> Game.Piece.O | O -> Game.Piece.X
    in
    List.length (winning_moves ~me:piece game)
    - List.length (moves_where_my_opponent_wins ~me:opponent game)
  ;;

  let rec minimax ~game ~depth ~piece ~maximizing_player =
    match equal depth 0 with
    | true -> heuristic_function ~game ~piece
    | false ->
      (match evaluate game with
       | Game_over result ->
         (match result.winner with
          | Some winning_piece ->
            (match Game.Piece.equal piece winning_piece with
             | true -> Int.max_value
             | false -> Int.min_value)
          | None -> 0)
       | Illegal_move -> Int.min_value
       | Game_continues ->
         let open_moves = available_moves game in
         (match maximizing_player with
          | true ->
            let value = Int.min_value in
            List.fold ~init:value open_moves ~f:(fun value move ->
              let new_game =
                match piece with
                | X -> place_piece game ~piece:X ~position:move
                | O -> place_piece game ~piece:O ~position:move
              in
              Int.max
                value
                (minimax
                   ~game:new_game
                   ~depth:(depth - 1)
                   ~piece
                   ~maximizing_player:(not maximizing_player)))
          | false ->
            let value = Int.max_value in
            List.fold ~init:value open_moves ~f:(fun value move ->
              let new_game =
                match piece with
                | X -> place_piece game ~piece:O ~position:move
                | O -> place_piece game ~piece:X ~position:move
              in
              Int.min
                value
                (minimax
                   ~game:new_game
                   ~depth:(depth - 1)
                   ~piece
                   ~maximizing_player:(not maximizing_player)))))
  ;;

  let best_next_move_minimax ~game ~piece =
    let possible_next_moves = available_moves game in
    let next_game_states =
      List.map possible_next_moves ~f:(fun position ->
        place_piece game ~piece ~position)
    in
    let order_of_best_moves =
      List.sort next_game_states ~compare:(fun game_1 game_2 ->
        Int.min
          (minimax ~game:game_1 ~depth:3 ~piece ~maximizing_player:true)
          (minimax ~game:game_2 ~depth:3 ~piece ~maximizing_player:true))
    in
    let next_move_opt = List.last order_of_best_moves in
    match next_move_opt with
    | None -> failwith "Game over, no moves left"
    | Some move -> move
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
         let evaluation = evaluate non_win in
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

  (* let%expect_test "exercise_three" =

     let winning_moves = winning_moves ~me:O non_win in print_s [%sexp
     (winning_moves : Game.Position.t list)];

     [%expect {| (((row 0) (column 2)) ((row 1) (column 2))) |}];

     return ()

     ;; *)

  let%expect_test "exercise_six" =
    print_game non_win;
    printf "\n\n";
    let best_move = best_next_move_minimax ~piece:O ~game:non_win in
    (* print_s [%sexp (winning_moves : Game.t)]; *)
    print_game best_move;
    [%expect {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X

      X |   |
      ---------
      O |   |
      ---------
      O | O | X |}];
    return ()
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
         let losing_moves = moves_where_my_opponent_wins ~me:piece non_win in
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
