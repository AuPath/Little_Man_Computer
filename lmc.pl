% Grassi Marco 829664

%translates File into machine code,
%creates an initial state where this translation + padding
%is the Memory and the input is the argument Input,
%if the execution is succesful returns Output
lmc_run(File, Input, Output) :-
    consult('parser.pl'),
    lmc_load(File, Mem), %defined in "Parser.pl"
    check_initial_memory(Mem, NewMem),
    execution_loop(state(0, 0, NewMem, Input, [], noflag) , Output).

%if the state is a valid state continue execution
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Output) :-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState), !,
    execution_loop(NewState, Output), !.

%if the state is halted stop
execution_loop(halted_state(_, _, _, _, Out, _), Out).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
    get_cell(Pc, Mem, Content),
    decode_instruction(Content, state(Acc, Pc, Mem, In, Out, Flag), NewState).

decode_instruction(901, state(Acc, Pc, Mem, In, Out, Flag), NewState):-
    instruction_input(state(Acc, Pc, Mem, In, Out, Flag), NewState).

decode_instruction(902, state(Acc, Pc, Mem, In, Out, Flag), NewState):-
    instruction_output(state(Acc, Pc, Mem, In, Out, Flag), NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 1, MemAdd), !,
    instruction_addition(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 2, MemAdd), !,
    instruction_subtraction(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 3, MemAdd), !,
    instruction_store(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content, _, _):-
    decode_further(Content, 4, _), !,
    writeln("Illegal instruction!"),
    writeln("Aborting execution!"),
    false.

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 5, MemAdd), !,
    instruction_load(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 6, MemAdd), !,
    instruction_branch(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 7, MemAdd), !,
    instruction_branch_if_zero(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 8, MemAdd), !,
    instruction_branch_if_positive(state(Acc, Pc, Mem, In, Out, Flag), MemAdd, NewState).

decode_instruction(Content,
                   state(Acc, Pc, Mem, In, Out, Flag),
                   NewState):-
    decode_further(Content, 0, _), !,
    instruction_halt(state(Acc, Pc, Mem, In, Out, Flag), NewState).

%Input
instruction_input(state(_, _, _, [], _, _), _):-
    writeln("Empty input queue!"), !,
    fail.

instruction_input(state(_, Pc, Mem, In, Out, Flag),
                  state(NewAcc, NewPc, Mem, NewIn, Out, Flag)):-
    nth0(0, In, NewAcc, NewIn),
    increment_Pc(Pc, NewPc).


%Output
instruction_output(state(Acc, Pc, Mem, In, Out, Flag),
                   state(Acc, NewPc, Mem, In, NewOut, Flag)):-
    append(Out, [Acc], NewOut),
    increment_Pc(Pc, NewPc).


%Addition
instruction_addition(state(Acc, Pc, Mem, In, Out, _),
                     MemAdd,
                     state(NewAcc, NewPc, Mem, In, Out, noflag)):-
    get_cell(MemAdd, Mem, Content),
    Temp is Acc + Content,
    Temp < 1000, !,
    NewAcc is mod(Temp, 1000),
    increment_Pc(Pc, NewPc).

instruction_addition(state(Acc, Pc, Mem, In, Out, _),
                     MemAdd,
                     state(NewAcc, NewPc, Mem, In, Out, flag)):-
    get_cell(MemAdd, Mem, Content),
    Temp is Acc + Content,
    NewAcc is mod(Temp, 1000),
    increment_Pc(Pc, NewPc).

%Subtraction
instruction_subtraction(state(Acc, Pc, Mem, In, Out, _),
                     MemAdd,
                     state(NewAcc, NewPc, Mem, In, Out, noflag)):-
    get_cell(MemAdd, Mem, Content),
    Temp is Acc - Content,
    Temp >= 0, !,
    NewAcc is mod(Temp, 1000),
    increment_Pc(Pc, NewPc).

instruction_subtraction(state(Acc, Pc, Mem, In, Out, _),
                     MemAdd,
                     state(NewAcc, NewPc, Mem, In, Out, flag)):-
    get_cell(MemAdd, Mem, Content),
    Temp is Acc - Content,
    NewAcc is mod(Temp, 1000),
    increment_Pc(Pc, NewPc).


%Store
instruction_store(state(Acc, Pc, Mem, In, Out, Flag),
                     MemAdd,
                     state(Acc, NewPc, NewMem, In, Out, Flag)):-
    replace_at_position(MemAdd, Mem, Acc, NewMem),
    increment_Pc(Pc, NewPc).

%Load
instruction_load(state(_, Pc, Mem, In, Out, Flag),
                     MemAdd,
                     state(NewAcc, NewPc, Mem, In, Out, Flag)):-
    nth0(MemAdd, Mem, NewAcc, _),
    increment_Pc(Pc, NewPc).

%Branch
instruction_branch(state(Acc, _, Mem, In, Out, Flag),
                     MemAdd,
                     state(Acc, NewPc, Mem, In, Out, Flag)):-
    NewPc is MemAdd.

%Branch if zero
instruction_branch_if_zero(state(Acc, _, Mem, In, Out, Flag),
                     MemAdd,
                     state(Acc, NewPc, Mem, In, Out, Flag)):-
    Acc = 0,
    Flag = noflag, !,
    NewPc is MemAdd.

instruction_branch_if_zero(state(Acc, Pc, Mem, In, Out, Flag),
                     _,
                     state(Acc, NewPc, Mem, In, Out, Flag)):-
    increment_Pc(Pc, NewPc).


%Branch if positive
instruction_branch_if_positive(state(Acc, _, Mem, In, Out, Flag), MemAdd, state(Acc, NewPc, Mem, In, Out, Flag)):-
   Flag = noflag, !,
   NewPc is MemAdd.

instruction_branch_if_positive(state(Acc, Pc, Mem, In, Out, Flag), _, state(Acc, NewPc, Mem, In, Out, Flag)):-
    increment_Pc(Pc, NewPc).

%Halt
instruction_halt(state(Acc, Pc, Mem, In, Out, Flag),
                 halted_state(Acc, Pc, Mem, In, Out, Flag)).

%HELPER PREDICATES


get_cell(Adrs, Mem, Content) :-
    nth0(Adrs, Mem, Content, _).

%decode_further(Content, Itype, MemAdd)
%based on a three digit number Itype is the first,
%MemAdd the last two (Es 142 -> 1 43)
decode_further(Content, Itype, MemAdd):-
    Temp is Content/100,
    Itype is truncate(Temp),
    MemAdd is mod(Content,100).

%increment_Pc(Pc,NewPc).
increment_Pc(99, 0).
increment_Pc(Pc, NewPc):-
    NewPc is (Pc+1).

%if the program does not fit into memory error
check_initial_memory(Mem, _):-
    length(Mem, L),
    L > 100,
    fail.

%if the file is empty (but legal)
check_initial_memory([], NewMem):-
    build_list(0, 100, NewMem).

%if the program does not occupy the whole memory
%a padding of halt instructions is added.
check_initial_memory(Mem, NewMem):-
    length(Mem, L),
    L1 is 100-L,
    build_list(0, L1, Mem2),
    append(Mem, Mem2, NewMem).

replace_at_position(Pos, List, Elem, NewList):-
    nth0(Pos, List, _, Rest),
    nth0(Pos, NewList, Elem, Rest).

%build_list(Value,Length,List) returns a
%List of Length made of Value.
build_list(_,0,[]).
build_list(Value, L, [Value | Rest]):-
    L > 0,
    L1 is L-1,
    build_list(Value, L1, Rest).
