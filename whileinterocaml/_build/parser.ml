exception Error

type token = 
  | WHILE
  | THEN
  | SUB
  | SEMI
  | RPAREN
  | RBRACK
  | OR
  | NOT
  | MUL
  | LT
  | LPAREN
  | LBRACK
  | INT of (int)
  | IF
  | ID of (string)
  | GT
  | EQ
  | EOF
  | ELSE
  | DO
  | DIV
  | BOOL of (bool)
  | ASSIGN
  | AND
  | ADD

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState50
  | MenhirState45
  | MenhirState42
  | MenhirState40
  | MenhirState36
  | MenhirState34
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState0

  
    open Syntax
let _eRR =
  Error

let rec _menhir_goto_separated_nonempty_list_SEMI_stmtone_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Syntax.stmt list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMI_stmtone_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState34 | MenhirState40 | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Syntax.stmt list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_stmtone__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_stmtone : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | IF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | EOF | RBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Syntax.stmt list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_SEMI_stmtone_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (Syntax.term) =                             ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | EOF | EQ | GT | LT | OR | RBRACK | RPAREN | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Sub (eone, etwo) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
        let _v : (Syntax.term) =                                 ( Mul (eone, etwo) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
        let _v : (Syntax.term) =                                 ( Div (eone, etwo) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | EOF | OR | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Or (eone, etwo)  ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GT | LT | OR | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Less (eone, etwo) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | EOF | EQ | GT | LT | OR | RBRACK | RPAREN | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Add (eone, etwo) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GT | LT | OR | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Greater (eone, etwo) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GT | LT | OR | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( Eq (eone, etwo)   ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.term) =                                 ( And (eone, etwo) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _v : (Syntax.term) =                                 ( Not e ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _v : (Syntax.term) =                                 ( Neg e            ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACK ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                    | IF ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                    | RBRACK ->
                        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | THEN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACK ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                    | IF ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | RBRACK ->
                        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | EOF | RBRACK | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, s), _, e) = _menhir_stack in
            let _v : (Syntax.stmt) =   ( Assign (Var s, e) ) in
            _menhir_goto_stmtone _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMI_stmtone__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (Syntax.stmt) = let ds =
      let xs = xs0 in
          ( xs )
    in
                                          ( List ds ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACK ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                    | IF ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                    | RBRACK ->
                        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, t), _, eone), _, etwo) = _menhir_stack in
            let _v : (Syntax.stmt) =   ( If (t, eone, etwo) ) in
            _menhir_goto_stmtone _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, t), _, e) = _menhir_stack in
            let _v : (Syntax.stmt) =   ( While (t, e) ) in
            _menhir_goto_stmtone _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, ds) = _menhir_stack in
            let _v : (Syntax.stmt) =                     (ds) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let n = _v in
    let _v : (Syntax.term) =             ( Int n ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let _v : (Syntax.term) =             ( Var s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _v : (Syntax.term) =             ( Bool b ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.stmt list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_SEMI_stmtone__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NOT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | NOT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
      }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.stmt) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
      } in
    Obj.magic (let _menhir_stack = () in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



