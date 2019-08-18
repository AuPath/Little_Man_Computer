% Grassi Marco 829664

translate_instruction("ADD",100).
translate_instruction("SUB",200).
translate_instruction("STA",300).
translate_instruction("LDA",500).
translate_instruction("BRA",600).
translate_instruction("BRZ",700).
translate_instruction("BRP",800).
translate_instruction("DAT",0).

translate_instruction("INP",901).
translate_instruction("OUT",902).
translate_instruction("HLT",000).

instruction_with_argument("ADD").
instruction_with_argument("SUB").
instruction_with_argument("STA").
instruction_with_argument("LDA").
instruction_with_argument("BRA").
instruction_with_argument("BRZ").
instruction_with_argument("BRP").

instruction_without_argument("INP").
instruction_without_argument("OUT").
instruction_without_argument("HLT").

lmc_load(File, FinalMem):-
    reset(),
    open(File,read,Stream),
    read_string(Stream, _, Whole_String),
    split_string(Whole_String,"\n", "", Formatted_Whole_List),
    translate_rows(Formatted_Whole_List, 0, Mem), !,
    check_unresolved_labels(Mem, FinalMem), !,
    close(Stream).

%translate_rows/3(Row,Row_Number,Translation)
%The row is correctly formatted, is translated into it's numerical
%counterpart and is then added to the Memory (the output)
%if the row is a comment the row number is not increased
translate_rows([H | T], Row_Number, [Recognized_Instruction | Y]):-
    handle_comment_without_space(H, RowString),
    string_upper(RowString, Upper_String),
    split_string(Upper_String, " ", " ", Row_List),
    recognize(Row_List, Row_Number, X),
    check_for_comment(X, Recognized_Instruction),
    Next_Row_Number is Row_Number + 1,
    translate_rows(T, Next_Row_Number, Y).

translate_rows([H | T], Row_Number, Y):-
    handle_comment_without_space(H, RowString),
    string_upper(RowString, Upper_String),
    split_string(Upper_String, " ", " ", Row_List),
    recognize(Row_List, Row_Number, _),
    translate_rows(T, Row_Number, Y).

translate_rows([], _,[]).

%If there are unresolved labels resolve them
check_unresolved_labels(Mem, FinalMem):-
    current_predicate(unresolved_label/2),
    unresolved_label(Label_Name, Position),
    current_predicate(label/2),
    label(Label_Name, Label_Value),
    nth0(Position, Mem, Old_Value, Rest_List),
    New_Value is Old_Value + Label_Value,
    nth0(Position, NewMem, New_Value, Rest_List),
    retract(unresolved_label(Label_Name, Position)),
    check_unresolved_labels(NewMem, FinalMem).

%if there isn't a label that corresponds
%to the unresolved-label then fail
check_unresolved_labels(Mem, Mem):-
     current_predicate(unresolved_label/2),
     unresolved_label(_, _), !,
     fail.

%if there aren't any unresolved labels
%return the memory as is
check_unresolved_labels(Mem, Mem).

%PARSING FSM (An image of the Automata is included in
%this file's directory)

initial(q0).
final(q1).
final(q4).
final(q6).
final(q7).

%delta/5(State, Input, NewState, Row_Number, Output)
%each state transition produces a numerical output,
%0 is the neutral output which doesn't influence the
%previous and next transition/translation
%Row_Number is used to assign a value to labels.

%HANDLING OF COMMENT STRING "//" AND EMPTY LINE
%A transition from the starting state q0 to one of
%the final states q7 produces -99. This value isn't
%obtainable by any other transition and signifies that
%the translated row is a comment and as such must not
%increase the row number.

delta(q1, "//", q7, _, Output):-
    Output is 0.

delta(q6, "//", q7, _, Output):-
    Output is 0.

delta(q7, _, q7, _, Output):-
    Output is 0.

delta(q0, "//", q7, _, Output):-
    Output is -99.

delta(q0, "", q7, _, Output):-
    Output is -99.

%CASE FOR DAT WITH POSSIBLE 0-999 ARGUMENT

delta(q0,"DAT", q4, _, Output):-
    Output is 0.

delta(q4, X, q6, Row_Number, Output):-
    check_dat_argument(X, Row_Number, Output).

delta(q4, "//", q7, _, Output):-
    Output is 0.

% CASES FOR THE INSTRUCTIONS WITH ARGUMENTS

delta(q0, X, q3, _, Output):-
    recognize_instruction_with_argument(X, Output).


% CASES FOR THE INSTRUCTIONS WITHOUT ARGUMENTS

delta(q0, X, q1, _, Output):-
    recognize_instruction_without_argument(X, Output).

% CASE FIRST THING LABEL

delta(q0, X, q2, Row_Number, Output):-
    label_verification(X, Row_Number, Output), !.

% CASE LAST ARGUMENT

delta(q3, X, q6, Row_Number, Output):-
    check_argument(X, Row_Number, Output).

delta(q2, "DAT", q4, _, Output):-
    translate_instruction("DAT",Output).

delta(q2, X, q1, _, Output):-
    recognize_instruction_without_argument(X, Output).

delta(q2, X, q3, _, Output):-
    recognize_instruction_with_argument(X, Output).

%The input (one row) is put through a DFA,
%if it is accepted and therefore valid
%it's numerical translation is returned
recognize(Input, Row_Number, Translated_Instruction):-
    initial(S),
    accept(Input, S, Row_Number, Translated_Instruction).

%accept/3(Input, State, Row_Number, Output)
%the input/row is only accepted if by
%the end of the transitions all input was consumed
%and the DFA is in a final state.
accept([], Q, _, Out):-
    Out is 0,
    final(Q).

%the output of each delta-transition is
%cumulatively added and then returned.
accept([I | Is], S, Row_Number, Total):-
    delta(S, I, N, Row_Number, Output), !,
    accept(Is, N, Row_Number, Rest),
    Total is Output + Rest.



%HELPER PREDICATES

%if the instruction is an instruction without argument
%it its numerical conversion is returned
recognize_instruction_without_argument(X, Out):-
    instruction_without_argument(X), !,
    translate_instruction(X, Out).

%if the instruction is an instruction with argument
%it its numerical conversion is returned
recognize_instruction_with_argument(Inp, Out):-
    instruction_with_argument(Inp), !,
    translate_instruction(Inp, Out).

valid_number_argument(X, Y):-
    number_string(Y, X),
    integer(Y),
    Y >=0,
    Y =< 99.

%a valid label begins with a letter
check_valid_label(Label_Name, _):-
    string_chars(Label_Name, Y),
    nth0(0, Y, First),
    char_type(First, alpha).

%checks if the label is well formed
%then if it it is already defined then fail
label_verification(X, Row_Number, _) :-
    check_valid_label(X, Row_Number),
    current_predicate(label/2),
    label(X,_), !,
    fail.

%if it is well formed and hasn't already been defined
%the label is defined with the structure (Name, Value)
label_verification(X, Row_Number, Output) :-
    check_valid_label(X, Row_Number), !,
    assert(label(X, Row_Number)),
    Output is 0.

%check_argument(Arg, Row_Number, Output)
%returns the argument as a number if it is between 0-99
check_argument(X, _, Output):-
    valid_number_argument(X, Y), !,
    Output is Y.

%if the argument is an already defined label it is
%converted to its numerical value.
check_argument(X, Row_Number, Output):-
    check_valid_label(X, Row_Number),
    current_predicate(label/2),
    label(X,Y), !,
    Output is Y.

%If the label is in a valid format but has not been declared
%it is added to the knowledge base to be processed later.
check_argument(X, Row_Number, Output):-
    check_valid_label(X, Row_Number), !,
    Output is 0,
    assert(unresolved_label(X, Row_Number)).

%allows number between 0 and 999
check_dat_argument(Arg, _, Out):-
    number_string(Out, Arg),
    integer(Out),
    between(0, 999, Out).

%Used to wipe the knowledge base of all possibly defined
%labels between executions
reset():-
    retractall(label(_, _)),
    retractall(unresolved_label(_, _)).

check_for_comment(X, X):-
    X >= 0.

%if the string contains a comment a space is added before and
%after the //. If the string does not contain // it is returned
%as is.
handle_comment_without_space(String, NewString):-
    sub_string(String, B, L, _, "//"), !,
    string_codes(String, String_As_List),
    char_code(' ', Code),
    nth0(B, TempList, Code, String_As_List),
    Pos is B+L+1,
    nth0(Pos, NewList, Code, TempList),
    string_codes(NewString, NewList).

handle_comment_without_space(String, String).
