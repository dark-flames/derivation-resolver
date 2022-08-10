#[macro_export]
macro_rules! rules {
    [$(($id: ident, $name: literal)),*] => {
        $(
        pub const $id: crate::derive::RuleName = $name;
        )*
    }
}

rules![
    (E_INT, "E-Int"),
    (E_BOOL, "E-Bool"),
    (E_PLUS, "E-Plus"),
    (E_MINUS, "E-Minus"),
    (E_TIMES, "E-Times"),
    (E_LT, "E-Lt"),
    (E_IF_T, "E-IfT"),
    (E_IF_F, "E-IfF"),
    (E_LET, "E-Let"),
    (E_FUN, "E-Fun"),
    (E_APP, "E-App"),
    (E_LET_REC, "E-LetRec"),
    (E_APP_REC, "E-AppRec"),
    (E_NIL, "E-Nil"),
    (E_CONS, "E-Cons"),
    (E_MATCH_NIL, "E-MatchNil"),
    (E_MATCH_CONS, "E-MatchCons"),
    (B_PLUS, "B-Plus"),
    (B_MINUS, "B-Minus"),
    (B_TIMES, "B-Times"),
    (B_LT, "B-Lt"),
    //typed
    (T_INT, "T-Int"),
    (T_BOOL, "T-Bool"),
    (T_VAR, "T-Var"),
    (T_IF, "T-If"),
    (T_PLUS, "T-Plus"),
    (T_MINUS, "T-Minus"),
    (T_MULT, "T-Mult"),
    (T_LT, "T-Lt"),
    (T_LET, "T-Let"),
    (T_ABS, "T-Abs"),
    (T_APP, "T-App"),
    (T_LET_REC, "T-LetRec"),
    (T_NIL, "T-Nil"),
    (T_CONS, "T-Cons"),
    (T_MATCH, "T-Match")
];
