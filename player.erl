-module(player).
-export([init/3]).

init(Name, Credits, MasterPid) ->
  Seed = erlang:phash2({self(), Name, os:system_time(nanosecond)}),
  rand:seed(exsplus, {Seed, Seed, Seed}),
  timer:sleep(200),
  player_loop(Name, 0, Credits, MasterPid).

player_loop(Name, UsedCredits, Credits, MasterPid) ->
  random_delay(),
  make_request(Name),
  receive
    {start_game, From} ->
      case whereis(From) of
        undefined ->
          player_loop(Name, UsedCredits, Credits, MasterPid);
        _ ->
          MasterPid ! {request_game, Name, From},
          player_loop(Name, UsedCredits, Credits, MasterPid)
      end;
    {game_id, GameId, Opponent} ->
      case whereis(Opponent) of
        undefined ->
          player_loop(Name, UsedCredits, Credits, MasterPid);
        _ ->
          Move = random_rps(),
          case whereis(Opponent) of
            undefined ->
              player_loop(Name, UsedCredits, Credits, MasterPid);
            _ ->
              Opponent ! {rps_move, GameId, Move, Name},
              player_wait_move(Name, Move, UsedCredits, Credits, MasterPid, GameId,Opponent, 2)
          end
      end;
    tournament_over ->
      unregister(Name),
      halt()
  after 1000 ->
    player_loop(Name, UsedCredits, Credits, MasterPid)
  end.

player_wait_move(Name, Move, UsedCredits, Credits, MasterPid, GameId, Opponent, RetryCount) ->
  receive
    {rps_move, GameId, OpponentMove, From} ->
      Result = determine_winner(OpponentMove, Move),
      case Result of
        lose ->
          if
            Credits - 1 =:= 0 ->
              MasterPid ! {player_out, From, Move, Name, OpponentMove, Name},
              unregister(Name);
            true ->
              MasterPid ! {game_result, GameId, Name, From, Result,OpponentMove,Move, UsedCredits, Credits},
              player_loop(Name, UsedCredits + 1, Credits - 1, MasterPid)
          end;
      tie ->
        MasterPid ! {game_result, GameId, Name, From, Result,OpponentMove,Move, UsedCredits, Credits},
        case whereis(From) of
          undefined ->
            player_loop(Name, UsedCredits, Credits, MasterPid);
          _ ->
            NewMove = random_rps(),
            From ! {rps_move, GameId, NewMove, Name},
            player_wait_move(Name, NewMove, UsedCredits, Credits, MasterPid, GameId, Opponent, RetryCount)
        end;
      _->
        player_loop(Name, UsedCredits, Credits, MasterPid)
      end;
    tournament_over ->
      unregister(Name),
      halt()
  after 1000 ->
    if RetryCount > 0 ->
      player_wait_move(Name, Move, UsedCredits, Credits, MasterPid, GameId, Opponent, RetryCount - 1);
      true ->
        player_loop(Name, UsedCredits, Credits, MasterPid)
    end
  end.


random_rps() ->
  Moves = [rock, paper, scissors],
  lists:nth(rand:uniform(length(Moves)), Moves).

determine_winner(rock, scissors) -> win;
determine_winner(paper, rock) -> win;
determine_winner(scissors, paper) -> win;
determine_winner(Move, Move) -> tie;
determine_winner(_, _) -> lose.

random_delay() ->
  timer:sleep(rand:uniform(90) + 10).

make_request(Name) ->
  OtherPlayers = [P || P <- erlang:registered(), P =/= Name, lists:prefix("player_", atom_to_list(P))],
  if
    OtherPlayers =/= [] ->
      Target = lists:nth(rand:uniform(length(OtherPlayers)), OtherPlayers),
      case whereis(Target) of
        undefined ->
          io:format("Target player ~p is not available for game request~n", [Target]);
        _ ->
          Target ! {start_game, Name}
      end;
    true -> ok
  end.