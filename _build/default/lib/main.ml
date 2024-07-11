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
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
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

  let check_row ~game ~(position : Game.Position.t) ~length_to_check_for =
    let game_board, _win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let entire_row_opt =
      List.init length_to_check_for ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row; column = check_col + length })
    in
    let entire_row = List.filter_opt entire_row_opt in
    match List.all_equal entire_row ~equal:Game.Piece.equal with
    | Some _ -> equal (List.length entire_row) length_to_check_for
    | None -> false
  ;;

  let check_col ~game ~(position : Game.Position.t) ~length_to_check_for =
    let game_board, _win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let entire_col_opt =
      List.init length_to_check_for ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row + length; column = check_col })
    in
    let entire_col = List.filter_opt entire_col_opt in
    match List.all_equal entire_col ~equal:Game.Piece.equal with
    | Some _ -> equal (List.length entire_col) length_to_check_for
    | None -> false
  ;;

  let check_diagonal ~game ~(position : Game.Position.t) ~length_to_check_for
    =
    let game_board, _win_condition_length = get_info_for_evaluate ~game in
    let check_row = position.row in
    let check_col = position.column in
    let forward_diagonal_opt =
      List.init length_to_check_for ~f:(fun length ->
        Map.find
          game_board
          { Game.Position.row = check_row + length
          ; column = check_col + length
          })
    in
    let backward_diagnoal_opt =
      List.init length_to_check_for ~f:(fun length ->
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
      | Some _ -> equal (List.length forward_diagonal) length_to_check_for
      | None -> false
    in
    let win_via_backward_diagonal =
      match List.all_equal backward_diagnoal ~equal:Game.Piece.equal with
      | Some _ -> equal (List.length backward_diagnoal) length_to_check_for
      | None -> false
    in
    win_via_backward_diagonal || win_via_forward_diagonal
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let occupied_positions = Map.keys game.board in
    let _, win_condition_length = get_info_for_evaluate ~game in

    let position_from_where_someone_wins =
      List.filter occupied_positions ~f:(fun pos ->
        check_col
          ~game
          ~position:pos
          ~length_to_check_for:win_condition_length
        || check_row
             ~game
             ~position:pos
             ~length_to_check_for:win_condition_length
        || check_diagonal
             ~game
             ~position:pos
             ~length_to_check_for:win_condition_length)
    in
    match position_from_where_someone_wins with
    | [] -> (
      match (List.length occupied_positions) = (Int.pow win_condition_length 2) with 
      | true -> Game_over { winner = None }
      | false -> Game_continues
    )
      
    | h :: _ -> Game_over { winner = Map.find game.board h }
  ;;

  (* (match t with | [] -> Game_over { winner = Map.find game.board h } | _
     :: _ -> Illegal_move) *)

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

  (* checks for n in a row and returns the appropriate incentive *)
  let check_for_n_consecutive ~game ~position ~n =
    let bonus_row = check_row ~game ~position ~length_to_check_for:n in
    let bonus_col = check_col ~game ~position ~length_to_check_for:n in
    let bonus_diagonal =
      check_diagonal ~game ~position ~length_to_check_for:n
    in
    match bonus_row, bonus_col, bonus_diagonal with
    | true, true, true -> Int.pow n 4
    | true, true, _ | true, _, true | _, true, true -> Int.pow n 3
    | true, _, _ | _, true, _ | _, _, true -> Int.pow n 2
    | _ -> 0
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
    [%expect {||}];
    return ()
  ;;

  let max_depth = 9

  let _heuristic_function_1 ~(game : Game.t) ~(piece : Game.Piece.t) =
    match piece with
    | X ->
      List.length (winning_moves game ~me:X)
      - List.length (winning_moves game ~me:O)
    | O ->
      List.length (winning_moves game ~me:O)
      - List.length (winning_moves game ~me:X)
  ;;

  let _is_corner_position ~position ~(game : Game.t) =
    let _n = Game.Game_kind.board_length game.game_kind - 1 in
    match position with
    | { Game.Position.row = 2; column = 2 }
    | { Game.Position.row = 0; column = 0 }
    | { Game.Position.row = 2; column = 0 }
    | { Game.Position.row = 0; column = 2 } ->
      true
    | _ -> false
  ;;

  let _heuristic_function_2 ~(game : Game.t) ~(piece : Game.Piece.t) =
    let losing_move_punishment =
      50
      *
      match
        List.length (winning_moves game ~me:(Game.Piece.flip piece)) >= 2
      with
      | true -> -10
      | false -> 0
    in
    (* let occupied_positions = Map.keys game.board in
    let my_piece_positions = 0 in *)
      (* List.filter occupied_positions ~f:(fun position ->
        Game.Piece.equal (Map.find_exn game.board position) piece)
    in *)
    let bonus_for_consecutive = 0 in
      (* List.fold my_piece_positions ~init:0 ~f:(fun bonus_so_far position ->
        bonus_so_far
        + check_for_n_consecutive ~game ~position ~n:2
        + check_for_n_consecutive ~game ~position ~n:2
        + check_for_n_consecutive ~game ~position ~n:2)
    in *)
    let punish_for_consecutive = 0 in
      (* List.fold my_piece_positions ~init:0 ~f:(fun bonus_so_far position ->
        bonus_so_far
        + check_for_n_consecutive ~game ~position ~n:2
        + check_for_n_consecutive ~game ~position ~n:2
        + check_for_n_consecutive ~game ~position ~n:2)
    in *)
    bonus_for_consecutive - punish_for_consecutive + losing_move_punishment
  ;;

  (* match List.length (winning_moves game ~me:piece) >= 2 with | true ->
     100_000 | false -> (match List.length (winning_moves game
     ~me:(Game.Piece.flip piece)) >= 2 with | true -> Int.min_value | false
     -> let score_part_1 = 50 * -List.length (winning_moves game
     ~me:(Game.Piece.flip piece)) in let occupied_positions = Map.keys
     game.board in let my_piece_positions = List.filter occupied_positions
     ~f:(fun position -> let possible_piece = Map.find game.board position in
     match possible_piece with | Some other_piece -> Game.Piece.equal piece
     other_piece | None -> false) in let my_corner_positions = List.filter
     my_piece_positions ~f:(fun position -> is_corner_position ~game
     ~position) in let score_part_2 = 0 * List.length my_corner_positions in
     score_part_1 + score_part_2) *)

  let _heuristic_function_3 ~(game : Game.t) ~(piece : Game.Piece.t) =
    let open Game in
    match game.game_kind with
    | Game_kind.Tic_tac_toe -> _heuristic_function_2 ~game ~piece
    | Game_kind.Omok ->
      let losing_move_punishment =
        500
        *
        match piece with
        | X -> 0 - List.length (winning_moves game ~me:O)
        | O -> 0 - List.length (winning_moves game ~me:X)
      in
      let occupied_positions = Map.keys game.board in
      let my_piece_positions =
        List.filter occupied_positions ~f:(fun position ->
          Game.Piece.equal (Map.find_exn game.board position) piece)
      in
      let bonus_for_consecutive =
        List.fold my_piece_positions ~init:0 ~f:(fun bonus_so_far position ->
          bonus_so_far
          + check_for_n_consecutive ~game ~position ~n:2
          + check_for_n_consecutive ~game ~position ~n:2
          + check_for_n_consecutive ~game ~position ~n:2)
      in
      bonus_for_consecutive + losing_move_punishment
  ;;

  let get_best_next_move_list ~game ~piece =
    match List.length (winning_moves ~me:piece game) with
    | 0 ->
      (match
         List.length (winning_moves game ~me:(Game.Piece.flip piece))
       with
       | 0 -> available_moves game
       | _ -> winning_moves game ~me:(Game.Piece.flip piece))
    | _ -> winning_moves ~me:piece game
  ;;

  let _optimal_move_without_minimax ~game ~piece =
    match List.random_element (get_best_next_move_list ~game ~piece) with
    | Some move -> move
    | None -> failwith "no moves left"
  ;;

  let rec minimax ~game ~depth ~piece ~maximizing_player =

    (* print_s [%message "" (depth:int)]; *)
    
    let temp = 

    match equal depth 0 with
    | true -> _heuristic_function_3 ~game ~piece
    | false ->

      (match evaluate game with
       | Game_over result ->
         (match result.winner with
          | Some winning_piece ->
            (match Game.Piece.equal piece winning_piece with
             | true -> 100_000 + depth
             | false -> -100_000 - depth)
          | None -> 0)
       | Illegal_move ->
         print_endline "\nThis is out illegal game\n";
         print_game game;
         raise_s [%message "This is an illegal move" (game : Game.t)]
       | Game_continues ->
         let open_moves = available_moves game in
         (match maximizing_player with
          | true ->
            let value =  Int.min_value in
            List.fold ~init:value open_moves ~f:(fun value move ->
              let new_game = place_piece game ~piece ~position:move in
              Int.max
                value
                (minimax
                   ~game:new_game
                   ~depth:(depth - 1)
                   ~piece
                   ~maximizing_player:(not maximizing_player)))
          | false ->
            let value = Int.max_value in
            (* if (List.length (open_moves) = 0) then print_s [%message "this is a full board" (( evaluate game) : Game.Evaluation.t)] ; *)

            List.fold ~init:value open_moves ~f:(fun value move ->
              let new_game =
                place_piece
                  game
                  ~piece:(Game.Piece.flip piece)
                  ~position:move
              in
              Int.min
                value
                (minimax
                   ~game:new_game
                   ~depth:(depth - 1)
                   ~piece
                   ~maximizing_player:(not maximizing_player))))) 
                  in 
                  (* if depth <= 2 then (print_game game; print_s [%message "" (temp: int)]);  *)
                  temp  
  ;;

  let _is_empty_board ~(game : Game.t) =
    let occupied_positions = Map.keys game.board in
    Int.equal (List.length occupied_positions) 0
  ;;

  (* (match List.length (winning_moves game ~me:(Game.Piece.flip piece)) with
     | 0 -> winning_moves game ~me:(Game.Piece.flip piece) | _ ->
     available_moves game) *)

  (* (match List.length (winning_moves ~me:(Game.Piece.flip piece) game) with
     | 0 -> winning_moves ~me:(Game.Piece.flip piece) game | _ -> (match
     List.length (available_moves_that_do_not_immediately_lose ~me:piece
     game) with | 0 -> available_moves game | _ ->
     available_moves_that_do_not_immediately_lose ~me:piece game)) *)
  let best_next_move_minimax ~game ~piece : Game.Position.t =
    (* match is_empty_board ~game with
    | true -> { Game.Position.row = 1; column = 1 }
    | false -> *)
      let possible_next_moves =
        get_best_next_move_list ~game ~piece
        (* match List.length (winning_moves ~me:piece game) with | 0 ->
           winning_moves ~me:piece game | _ -> (match List.length
           (winning_moves ~me:(Game.Piece.flip piece) game) with | 0 ->
           winning_moves ~me:(Game.Piece.flip piece) game | _ -> (match
           List.length (available_moves_that_do_not_immediately_lose
           ~me:piece game) with | 0 -> available_moves game | _ ->
           available_moves_that_do_not_immediately_lose ~me:piece game)) *)
      in
      let next_game_states =
        List.map possible_next_moves ~f:(fun position ->
          position, place_piece game ~piece ~position)
      in
      let init_optimal_move =
        Int.min_value, List.hd_exn possible_next_moves
      in
      let optimal_move =
        List.fold
          ~init:init_optimal_move
          next_game_states
          ~f:(fun optimal_move next_game_state ->
            let curr_best_score, curr_best_move = optimal_move in
            let possibly_better_pos, possibly_better_game =
              next_game_state
            in
            let possibly_better_score =
              minimax
                ~game:possibly_better_game
                ~depth:max_depth
                ~piece
                ~maximizing_player:false
            in
            match Int.(possibly_better_score > curr_best_score) with
            | true -> possibly_better_score, possibly_better_pos
            | false -> curr_best_score, curr_best_move)
      in
      snd optimal_move
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
    (* print_game _empty_board;
    printf "\n\n"; *)
    let best_move = best_next_move_minimax ~piece:X ~game:_empty_board in
    (* print_s [%sexp (winning_moves : Game.t)]; *)
    let available_moves = available_moves _empty_board in
    List.iter available_moves ~f:(fun next_move ->
      let next_move_game =
        place_piece _empty_board ~piece:X ~position:next_move
      in
      print_game next_move_game;
      printf
        "\n%d "
        (minimax
           ~game:next_move_game
           ~depth:max_depth
           ~piece:X
           ~maximizing_player:false);
      printf "\n\n");
    let best_next_game_state =
      place_piece _empty_board ~piece:X ~position:best_move
    in
    print_game best_next_game_state;
    [%expect {| |}];
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

  let self_omok_test =
    Command.async
      ~summary:"Omok Against Self"
      (let%map_open.Command () = return () in
       fun () ->
         let iterator = List.init 225 ~f:(fun x -> x) in
         let _ =
           List.fold iterator ~init:empty_game ~f:(fun game num ->
             print_game game;
             let piece =
               match num % 2 with 0 -> Game.Piece.X | _ -> Game.Piece.O
             in
             let new_board =
              let next_pos = best_next_move_minimax ~game ~piece in
               place_piece game ~piece ~position:(next_pos)
             in
             match evaluate new_board with
             | Game.Evaluation.Game_continues -> new_board
             | Game.Evaluation.Game_over { winner = piece } ->
               Core.print_s [%sexp (piece : Game.Piece.t option)];
               let _ = failwith "game over" in
               new_board
             | Game.Evaluation.Illegal_move ->
               let _ = failwith "game over" in
               new_board)
         in
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "self_omok_test", self_omok_test
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

let handle_rpc (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  let current_game : Game.t = query.game in
  let current_piece = query.you_play in
  let next_position =
    Exercises.best_next_move_minimax ~game:current_game ~piece:current_piece
  in
  let response =
    { Rpcs.Take_turn.Response.piece = current_piece
    ; Rpcs.Take_turn.Response.position = next_position
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
       Tcp.Server.close_finished server);;

;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
