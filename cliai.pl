% -------------------
% LOAD LIBRARIES
% -------------------
:- use_module(library(json)).
:- use_module(library(pcre)).
:- use_module(library(random)).
:- use_module(library(readutil)). % Required for reading input from CLI

% -------------------
% LOAD INTENT DATA
% -------------------
:- dynamic intent_pattern/2.   % intent_pattern(Tag, Pattern)
:- dynamic intent_response/2.  % intent_response(Tag, Response)

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
    ;   writeln('Warning: Data.json not found. Please create it.')
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
% CLI INTERFACE
% -------------------

% Main entry point
start_cli :-
    writeln('------------------------------------------------'),
    writeln('        Simple Chatbot '),
    writeln('------------------------------------------------'),
    load_intent_data,
    writeln(''),
    writeln('Type "quit" or "exit" to stop.'),
    nl,
    chat_loop.

% Recursive loop for reading input
chat_loop :-
    write('You: '),
    flush_output(current_output),
    read_line_to_string(user_input, Input),
    
    % Check for exit commands
    (   is_exit(Input)
    ->  writeln('Bot: Goodbye!'),
        halt
    ;   chatbot_response(Input, Reply),
        format('Bot: ~w~n', [Reply]),
        nl,
        chat_loop % Recurse to keep chat alive
    ).

% Helper to check if user wants to quit
is_exit(Input) :-
    string_lower(Input, Lower),
    (Lower == "quit" ; Lower == "exit").

% -------------------
% AUTO START
% -------------------
:- initialization(start_cli).