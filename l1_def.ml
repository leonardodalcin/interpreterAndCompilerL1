type term =
        TmIntegerVal
    |   TmFalse
    |   TmTrue
    |   TmPlus of term * term
    |   TmMinus of term * term
    |   TmMult of term * term
    |   TmDiv of term * term
    |   TmLesser of term * term
    |   TmLesserOrEqual of term * term
    |   TmEqual of term * term
    |   TmNotEqual of term * term
    |   TmGreater of term * term
    |   TmGreaterOrEqual of term * term
    |   TmIf of term * term * term
    |   TmX of term
    |   TmApp of term * term
    |   TmFn of term
    |   TmLetX of term
    |   TmLetRec of term
