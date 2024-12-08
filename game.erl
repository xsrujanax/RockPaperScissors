-module(game).
-export([start/1]).

start([PlayerFile]) ->
  register(game_id_server, spawn(fun() -> game_id_server(0) end)), % Start the game ID server
  {ok, PlayerInfo} = file:consult(PlayerFile),
  io:format("* Rock, Paper Scissors World Championship *~nStarting game log...~n"),
  lists:foreach(fun({Name, Credits}) ->
    io:format("Player ~p started with ~p credits~n", [Name, Credits])
                end, PlayerInfo),
  MasterPid = self(),
  InitialPlayers = [{Name, 0, Credits} || {Name, Credits} <- PlayerInfo],
  spawn_players(InitialPlayers, MasterPid),
  timer:sleep(200),
  game_loop(InitialPlayers, 0, lists:map(fun({Name, _, _}) -> list_to_atom("player_" ++ atom_to_list(Name)) end, InitialPlayers), InitialPlayers).

% Spawn a process for each player
spawn_players([], _MasterPid) -> ok;
spawn_players([{Name, _, Credits} | Rest], MasterPid) ->
  PlayerName = list_to_atom("player_" ++ atom_to_list(Name)),
  PlayerPid = spawn(player, init, [PlayerName, Credits, MasterPid]),
  register(PlayerName, PlayerPid),
  spawn_players(Rest, MasterPid).

% Game loop
game_loop(Players, GameId, PlayerNames, InitialPlayers) ->
  if length(Players) =:= 1 ->
    [{Winner, _, _}] = Players,
    io:format("~nWe have a winner...~n** Tournament Report **~nPlayers:~n"),
    lists:foreach(
      fun({Name, Used, Remaining}) ->
        io:format("~p: credits used: ~p, credits remaining: ~p~n", [Name, Used, Remaining]),
        display_credits(InitialPlayers, Name)
      end,
      Players),
    io:format("---------~n"),
    io:format("Total games: ~p~n", [GameId]),
    io:format("Winner: ~p~n", [Winner]),
    io:format("See you next year...~n"),
    lists:foreach(fun(Name) ->
      case whereis(Name) of
        undefined -> ok;
        Pid -> Pid ! tournament_over
      end
                  end, PlayerNames),
    halt();
    true ->
      receive
        {request_game, From, To} ->
          case {whereis(From), whereis(To)} of
            {undefined, _} ->
              game_loop(Players, GameId, PlayerNames, InitialPlayers);
            {_, undefined} ->
              game_loop(Players, GameId, PlayerNames, InitialPlayers);
            _ ->
              NewGameId = get_new_game_id(),
              io:format("~n+[~p] new game for ~p -> ~p", [NewGameId, From, To]),
              From ! {game_id, NewGameId, To},
              To ! {game_id, NewGameId, From},
              game_loop(Players, NewGameId, PlayerNames, InitialPlayers)
          end;
        {player_out, From, Move, Name, OpponentMove, Name} ->
          NameStr = atom_to_list(Name),
          PlayerName = string:sub_string(NameStr, 8),
          UpdatedPlayers = lists:filter(fun({N, _, _}) -> N =/= list_to_atom(PlayerName) end, Players),
          UpdatedInitialPlayers = update_initial_players(InitialPlayers, PlayerName),
          io:format("~n- (~p) ~p:~p -> ~p:~p = ~p loses [~p credits left]", [GameId, From, Move, Name, OpponentMove, Name, 0]),
          game_loop(UpdatedPlayers, GameId, PlayerNames, UpdatedInitialPlayers);
        Msg ->
          FormattedString = io_lib:format("~p", [Msg]),
          WithoutBraces = string:replace(string:replace(FormattedString, "{", ""), "}", ""),
          [_, GameIdStr, NameBin, FromBin, ResultBin, OpponentMovebin, MoveBin, UsedCreditsStr, CreditsStr] = re:split(WithoutBraces, ","),
          GameId2 = list_to_integer(binary_to_list(GameIdStr)),
          UsedCredits = list_to_integer(binary_to_list(UsedCreditsStr)),
          Credits = list_to_integer(binary_to_list(CreditsStr)),
          Result = list_to_atom(binary_to_list(ResultBin)),
          Name = list_to_atom(binary_to_list(NameBin)),
          From = list_to_atom(binary_to_list(FromBin)),
          OpponentMove = list_to_atom(binary_to_list(OpponentMovebin)),
          Move = list_to_atom(binary_to_list(MoveBin)),

          case Result of
            lose ->
              io:format("~n$ (~p) ~p:~p -> ~p:~p = ~p loses [~p credits left]", [GameId2, From, Move, Name, OpponentMove, Name, Credits - 1]),
              UpdatedPlayers = update_credits(Players, Name, UsedCredits + 1, Credits - 1),
              game_loop(UpdatedPlayers, GameId2, PlayerNames, InitialPlayers);
            tie ->
              io:format("$ (~p) ~p:~p -> ~p:~p = tie, ", [GameId2, From, Move, Name, OpponentMove]),
              game_loop(Players, GameId2, PlayerNames, InitialPlayers);
            _ ->
              game_loop(Players, GameId2, PlayerNames, InitialPlayers)
          end
      after 1000 ->
        game_loop(Players, GameId, PlayerNames, InitialPlayers)
      end
  end.

% Game ID server loop
game_id_server(CurrentId) ->
  receive
    {get_id, Caller} ->
      NewId = CurrentId + 1,
      Caller ! {game_id, NewId},
      game_id_server(NewId)
  end.

get_new_game_id() ->
  game_id_server ! {get_id, self()},
  receive
    {game_id, NewId} -> NewId
  end.

update_initial_players(InitialPlayers, PlayerName) ->
  PlayerAtom = list_to_atom(PlayerName),
  lists:map(fun({N, Used, Remaining}) ->
    case N of
      PlayerAtom ->
        {N, Remaining, 0};
      _ ->
        {N, Used, Remaining}
    end
            end, InitialPlayers).

update_credits(Players, Name, UsedCredits, Credits) ->
  lists:map(fun({N, Used, Remaining}) ->
    PlayerAtom = list_to_atom("player_" ++ atom_to_list(N)),
    case PlayerAtom of
      Name ->
        {N, UsedCredits, Credits};
      _ ->
        {N, Used, Remaining}
    end
            end, Players).

display_credits(Players, Name1) ->
  lists:foreach(
    fun({Name, Used, Remaining}) ->
      case Name =/= Name1 of
        true ->
          io:format("~p: credits used: ~p, credits remaining: ~p ~n", [Name, Used, Remaining]);
        false ->
          ok
      end
    end, Players).
