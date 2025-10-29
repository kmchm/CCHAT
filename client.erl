-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

% A wrapper for sending requests
% DestinationPID is of type atom
% Body is of type tuple
send(DestinationPID, Body) ->
    try genserver:request(DestinationPID, Body) of
        Response -> Response
        % take the response and return it
    catch
        _:_ -> {error, server_not_reached, "Error: Server could not be reached."}
        % catch all error
    end.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case send(St#client_st.server, {join, self(), St#client_st.nick, Channel}) of
        ok ->
            NewChannelList = [Channel | St#client_st.channels], % add the new channel to the list of channels
            {reply, ok, St#client_st{channels=NewChannelList}};
        Error ->
            {reply, Error, St}
        end;

% Leave channel
handle(St, {leave, Channel}) ->
    % check if the chanel is in the list
    case lists:member(Channel, St#client_st.channels) of
        true ->
            Data = send(list_to_atom(Channel), {leave, self()}), % send a leave request
            NewChannelsList = lists:delete(Channel, St#client_st.channels), % update the channel list
            {reply, Data, St#client_st{channels=NewChannelsList}}; % return the new tuple
        false ->
            {reply, {error, user_not_joined, "Error: Not a member of channel " ++ Channel}, St}
        end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Data = send(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg}),
    {reply, Data, St};

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
% send a request to change the nick
    case send(St#client_st.server, {nick, St#client_st.nick, NewNick}) of
        ok ->
            {reply, ok, St#client_st{nick=NewNick}};
        Error ->
            {reply, Error, St}
        end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % concurrently leave all channels
    lists:foreach(
        fun(Channel) ->
            spawn(fun() -> send(list_to_atom(Channel), {leave, self()}) end)
        end,
        St#client_st.channels
    ),
    % tell the server about the quit
    send(St#client_st.server, {quit, St#client_st.nick}),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
