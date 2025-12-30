% -------------------
% LOAD LIBRARIES
% -------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(json)).
:- use_module(library(pcre)).
:- use_module(library(random)).
 
% -------------------
% HTTP ROUTES
% -------------------
:- http_handler(root(chat), chat_handler, []).

% -------------------
% LOAD INTENT DATA
% -------------------
:- dynamic intent_pattern/2.   % intent_pattern(Tag, Pattern)
:- dynamic intent_response/2.  % intent_response(Tag, Response)
:- initialization(load_intent_data).

load_intent_data :-
    (   exists_file('Data.json')
    ->  open('Data.json', read, Stream),
        json_read_dict(Stream, Data),
        close(Stream),
        forall(
            member(Intent, Data.intents),
            (
                Tag = Intent.tag,
                % Store patterns
                forall(
                    member(Pattern, Intent.patterns),
                    (
                        string_lower(Pattern, PatternLower),
                        assertz(intent_pattern(Tag, PatternLower))
                    )
                ),
                % Store responses
                forall(
                    member(Response, Intent.responses),
                    assertz(intent_response(Tag, Response))
                )
            )
        ),
        writeln('Data.json loaded successfully.')
    ;   writeln('Warning: Data.json not found.')
    ).

% -------------------
% CHATBOT LOGIC
% -------------------

chatbot_response(Input, Reply) :-
    string_lower(Input, Lower),
    trim_string(Lower, Trimmed),
    (   % Find any intent where pattern is a substring
        intent_pattern(Tag, Pattern),
        sub_string(Trimmed, _, _, _, Pattern),
        findall(R, intent_response(Tag, R), Responses),
        random_member(Reply, Responses)
    ->  true
    ;   Reply = "I'm not sure about that. Ask me something else!"
    ).

trim_string(S, T) :-
    re_replace("^[ \\t\\n\\r]+|[ \\t\\n\\r]+$", "", S, T).

% -------------------
% API HANDLER
% -------------------

chat_handler(Request) :-
    option(method(post), Request),
    http_read_json_dict(Request, Dict),
    _{message: Msg} :< Dict,
    chatbot_response(Msg, Reply),
    reply_json_dict(_{reply: Reply}).

chat_handler(_) :-
    reply_json_dict(_{error: "Only POST allowed"}, [status(405)]).

% -------------------
% SERVER CONTROL
% -------------------
:- dynamic server/1.

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    assertz(server(Port)),
    format('Server running on http://localhost:~w/chat~n', [Port]).

stop_server :-
    server(Port),
    http_stop_server(Port, []),
    retract(server(Port)),
    writeln('Server stopped successfully.').

% -------------------
% AUTOMATIC SERVER START
% -------------------
:- initialization(start_server(8000)).
