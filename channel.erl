-module(channel).
-export([start/1, stop/1]).

-record(state, {
    name,     % channel name (string)
    members   % list of PIDs of clients who joined this channel
}).

initial_state(Name) ->
    #state{
        name = Name,
        members = []
    }.

% handle join channel client request
handle(St, {join, ClientPID}) ->
    % check if the client is already a member of the channel
    case lists:member(ClientPID, St#state.members) of
        true ->
            % if yes then return error with channel name
            {reply, {error, user_already_joined, "Already joined " ++ St#state.name}, St};
        false ->
            % add the new client PID to the the members list
            NewMembersList = [ClientPID | St#state.members],
            % return ok and update the state
            {reply, ok, St#state{members=NewMembersList}}
        end;

% handle leave channel client request
handle(St, {leave, ClientPID}) ->
    % remove the client PID from the members list
    NewMembersList = lists:delete(ClientPID, St#state.members),
    % return ok and update the state
    {reply, ok, St#state{members=NewMembersList}};

% handle sending message from a client to all channel members
handle(St, {message_send, ClientPID, ClientNick, Msg}) ->
    % check if the sender is actually a member of the channel
    case lists:member(ClientPID, St#state.members) of
        true ->
            % get list of all members except the sender
            OtherMembers = lists:delete(ClientPID, St#state.members),
            % prepare the body for recipients (genserver request data)
            Body = {message_receive, St#state.name, ClientNick, Msg},
            % send message to each member concurrently with genserver:request
            % a lot of concurrency here so the throughput passes
            lists:foreach(fun(Member) ->
                spawn(fun() ->
                    catch genserver:request(Member, Body)
                end)
            end, OtherMembers),
            {reply, ok, St};
        false ->
            % if sender is not a member of the channel
            {reply, {error, user_not_joined, "User not joined to channel"}, St}
    end;

% catch all other requests
handle(St, _) ->
    {reply, {error, not_implemented, "The request cannot be handled by the channel."}, St}.

% start a new channel process
start(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

% stop the channel process
stop(Name) ->
    genserver:stop(list_to_atom(Name)).
