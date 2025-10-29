-module(server).
-export([start/1,stop/1]).

% server state
-record(state, {
    nicks,    % List of all registered nicknames
    channels  % List of all active channels
}).

% initialize the server state
initial_state() ->
    #state{
        nicks = [],
        channels = []
    }.

% handle change nickname request
handle(St, {nick, PrevNick, NewNick}) ->
    % check if the new nickname is already taken
    case lists:member(NewNick, St#state.nicks) of
        % if newNick is already taken by someone else, i.e., not the same user
        true when PrevNick =/= NewNick ->
            % return error message that the nickname is already taken
            {reply, {error, nick_taken, NewNick ++ " is already taken."}, St};
        % if prevNick and newNick is the same thing or newnick is available, that is why _
        _ ->
            % update the nicknames list
            NewNickList = case PrevNick =:= NewNick of
                true -> St#state.nicks;  % no change if the same nick
                % else replace the previous nick with the new nick
                false -> [NewNick | lists:delete(PrevNick, St#state.nicks)]
            end,
            % return ok and update state with newNickList
            {reply, ok, St#state{nicks=NewNickList}}
        end;

% handle join channel request
handle(St, {join, ClientPID, ClientNick, Channel}) ->
    % check if a channel exists, if not create it and add to channels list
    NewChannelsList =
        case lists:member(Channel, St#state.channels) of
            true -> St#state.channels;  % channel already exists, no changes
            % channel doesn't exist so start a new channel process and add to the list
            false -> channel:start(Channel), [Channel | St#state.channels]
        end,
    % add client nickname to server's nick list if it does not have it yet
    NewNickList =
        case lists:member(ClientNick, St#state.nicks) of
            true -> St#state.nicks;  % Nick already registered, no change
            false -> [ClientNick | St#state.nicks]  % Add new nick to list
        end,
    % forward the join request to the right channel process
    ChannelResponse = genserver:request(list_to_atom(Channel), {join, ClientPID}),
    % return channel's response and update state with the new list
    {reply, ChannelResponse, St#state{nicks=NewNickList, channels=NewChannelsList}};

% handle quit client request
handle(St, {quit, ClientNick}) ->
    % remove the client nickname from the server nick list
    NewNickList = lists:delete(ClientNick, St#state.nicks),
    % return ok and update state without the quitting client nick
    {reply, ok, St#state{nicks=NewNickList}};

% handle get current server state request
handle(St, get_state) ->
    % return the current state without modifying it
    {reply, St, St};

% catch all requests handle
handle(St, _) ->
    {reply, {error, not_implemented, "The request cound not be processed."}, St}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % start a new server with the given atom name, initial state, and the handler function
    genserver:start(ServerAtom, initial_state(), fun handle/2).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    case whereis(ServerAtom) of
        undefined ->
            ok;
        _Pid ->
            try
                % stop all channels, then stop yourself
                State = genserver:request(ServerAtom, get_state),
                lists:foreach(fun(Channel) ->
                    try
                        genserver:stop(list_to_atom(Channel))
                    catch
                        _:_ -> ok
                    end
                end, State#state.channels),
                genserver:stop(ServerAtom)
            catch
                % if something goes wrong, just shut down the server
                _:_ ->
                    genserver:stop(ServerAtom)
            end
    end.
