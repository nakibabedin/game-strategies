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
        if Int.(column <> n - 1) then printf "| ";);
        
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
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =

    let n = Game.Game_kind.board_length game.game_kind in
    let rows = List.init n ~f:(fun row -> row) in
    (* let cols = List.init n ~f:(fun col -> col) in  *)
    
    let all_positions = List.concat_map rows ~f:(fun row -> 

      List.init n ~f:(fun col -> 
        { Game.Position.row = row; column = col})
      ) in 

      let occupied_positions = Map.keys game.board in
      
      List.filter all_positions ~f:(fun pos -> not (List.exists occupied_positions ~f:(Game.Position.equal pos)))


    (* ignore game;
    failwith "Implement me!" *)
  ;;

  (* let%expect_test "print_win_for_x" =
    List.iter (available_moves win_for_x) ~f:()
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;; *)

  (* let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;; *)

  let get_info_for_evaluate ~game = 
    let open Game in

    (* Win condition represents number of things in a row needed to win *)
    let win_condition_length = 
      match game.game_kind with 
      | Game_kind.Tic_tac_toe -> 3 
      | Game_kind.Omok -> 5
    in

    (game.board, win_condition_length)
  ;;

  let check_row ~game ~(position: Game.Position.t) =  

    let game_board, win_condition_length = get_info_for_evaluate ~game in
    
    let check_row = position.row in
    let check_col = position.column in 

    let entire_row_opt = List.init win_condition_length ~f:(
      fun length -> Map.find game_board {Game.Position.row=check_row; column=(check_col+length)}
    ) in 

    let entire_row = List.filter_opt entire_row_opt in 


    match (List.all_equal entire_row ~equal:(Game.Piece.equal)) with 
    | Some _ -> equal (List.length entire_row) win_condition_length
    | None -> false
    
  ;;

  let check_col ~game ~(position: Game.Position.t) = 

    let game_board, win_condition_length = get_info_for_evaluate ~game in
    
    let check_row = position.row in
    let check_col = position.column in 

    let entire_col_opt = List.init win_condition_length ~f:(
      fun length -> Map.find game_board {Game.Position.row=(check_row+length); column=check_col}
    ) in 

    let entire_col = List.filter_opt entire_col_opt in 

    match (List.all_equal entire_col ~equal:(Game.Piece.equal)) with 
    | Some _ -> equal (List.length entire_col) win_condition_length
    | None -> false
  
  ;; 

  let check_diagonal ~game ~(position: Game.Position.t)= 

    let game_board, win_condition_length = get_info_for_evaluate ~game in

    let check_row = position.row in
    let check_col = position.column in 

    let forward_diagonal_opt = List.init win_condition_length ~f:(
      fun length -> Map.find game_board {Game.Position.row=(check_row+length); column=(check_col+length)}
    ) in 
    let backward_diagnoal_opt = List.init win_condition_length ~f:(
      fun length -> Map.find game_board {Game.Position.row=(check_row-length); column=(check_col-length)}
    ) in 

    let forward_diagonal = List.filter_opt forward_diagonal_opt in 
    let backward_diagnoal = List.filter_opt backward_diagnoal_opt in 

    match (List.all_equal forward_diagonal ~equal:(Game.Piece.equal)) with 
    | Some _ -> equal (List.length forward_diagonal) win_condition_length
    | None -> (

      match (List.all_equal backward_diagnoal ~equal:(Game.Piece.equal)) with 
      | Some _ -> equal (List.length backward_diagnoal) win_condition_length
      | None -> false

    )

  ;;

  
  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t = 
  
    let occupied_positions =  Map.keys game.board in

    let winning_position = List.filter occupied_positions ~f:(
      fun pos -> 
        check_col ~game ~position:pos ||
        check_row ~game ~position:pos || 
        check_diagonal ~game ~position:pos
    ) in match winning_position with 
    | [] -> Game_continues
    | h :: t -> (
      match t with 
      | [] -> Game_over { winner=(Map.find game.board h) }
      | _ :: _ -> Illegal_move
    ) 

  ;;
(* 
  let%expect_test "print_non_win" =
    print_s "%s" (evaluate non_win);
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;; *)

  

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
