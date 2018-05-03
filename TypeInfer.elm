module TypeInfer exposing (..)

import Expr exposing (..)
import Helper
import Dict
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


type Type
    = TypeVar TVarName
    | BaseType Name
    | FuncType Type Type
    | ParamType Name (List Type)


type alias TVarName =
    Int


infixr 9 :>
(:>) =
    FuncType


int : Type
int =
    BaseType "int"



-- Inference Environment (IE)


type alias IE a =
    TypeData -> TVarEnv -> Result InferenceError ( a, TVarEnv )


type alias TypeData =
    Dict.Dict Name Type


type TVarEnv
    = TVarEnv
        -- nextTVarName
        TVarName
        -- constraints
        (Dict.Dict TVarName Type)


type InferenceError
    = CannotSetEqualError Type Type TVarEnv
    | MissingTypeInfo Name


emptyEnv : TVarEnv
emptyEnv =
    TVarEnv 0 Dict.empty


getEnv : IE TVarEnv
getEnv =
    \_ env -> Ok ( env, env )


newTVar : TVarEnv -> ( Type, TVarEnv )
newTVar (TVarEnv n d) =
    ( TypeVar n, TVarEnv (n + 1) d )


newTVarM : IE Type
newTVarM =
    \_ env -> Ok (newTVar env)


lookupTVar : TVarEnv -> TVarName -> Maybe Type
lookupTVar (TVarEnv _ d) n =
    Dict.get n d


envExtend : ( TVarName, Type ) -> TVarEnv -> TVarEnv
envExtend ( n, t ) (TVarEnv cur dict) =
    TVarEnv cur (Dict.insert n t dict)


envExtendM : ( TVarName, Type ) -> IE ()
envExtendM binding =
    \_ env -> Ok ( (), envExtend binding env )


return : a -> IE a
return a =
    \_ env -> Ok ( a, env )


fail : InferenceError -> IE a
fail err =
    \_ _ -> Err err


failCannotSetEqualError : Type -> Type -> IE a
failCannotSetEqualError t1 t2 =
    getEnv |> andThenIE (\env -> fail (CannotSetEqualError t1 t2 env))


andThenIE : (a -> IE b) -> IE a -> IE b
andThenIE f ma =
    \tdata env ->
        ma tdata env
            |> Result.andThen (\( a, env1 ) -> f a tdata env1)


batchIE : IE (IE a) -> IE a
batchIE mma =
    mma |> andThenIE identity


apIE : IE (a -> b) -> IE a -> IE b
apIE mf ma =
    mf |> andThenIE (\f -> ma |> andThenIE (f >> return))


mapIE : (a -> b) -> IE a -> IE b
mapIE f =
    andThenIE (return << f)


map2IE : (a -> b -> c) -> IE a -> IE b -> IE c
map2IE f =
    apIE << mapIE f


map3IE : (a -> b -> c -> d) -> IE a -> IE b -> IE c -> IE d
map3IE f a b c =
    apIE (map2IE f a b) c


unsafePrintEnv : IE ()
unsafePrintEnv =
    getEnv |> andThenIE (\env -> return (Helper.log [ showEnv env ] ()))


sequenceIEs : List (IE a) -> IE (List a)
sequenceIEs =
    List.foldr (map2IE (::)) (return [])


sequenceIEsNonempty : Nonempty (IE a) -> IE (Nonempty a)
sequenceIEsNonempty =
    Helper.nonemptyFoldr (map2IE (:::)) (mapIE Nonempty.fromElement)


sequenceIEsExprA : ExprA (IE a) -> IE (ExprA a)
sequenceIEsExprA =
    Expr.foldrA
        (\iea n -> mapIE (flip VarA n) iea)
        (\iea n -> mapIE (flip HoleA n) iea)
        (\iea f args -> map2IE (flip AppA f) iea (sequenceIEs args))
        (\iea x -> mapIE (flip LitA x) iea)
        (\iea c args -> map2IE (flip ConstructorA c) iea (sequenceIEs args))
        (\iea e cases -> map3IE CaseStmtA iea e (sequenceIEsNonempty cases))
        (\c params rhs -> mapIE (CaseA c params) rhs)



-- TYPE DATA OPERATIONS


getTData : IE TypeData
getTData =
    \tdata env -> Ok ( tdata, env )


lookupTypeData : Name -> IE Type
lookupTypeData varname =
    getTData
        |> andThenIE
            (\tdata ->
                case Dict.get varname tdata of
                    Just t ->
                        return t

                    Nothing ->
                        fail (MissingTypeInfo varname)
            )


lookupFreshTypeData : Name -> IE Type
lookupFreshTypeData =
    lookupTypeData >> andThenIE rewriteWithFreshTVars


runWithTypeDataExt : Name -> Type -> IE a -> IE a
runWithTypeDataExt n t =
    (>>) (Dict.insert n t)


runWithTypeDataExts : List ( Name, Type ) -> IE a -> IE a
runWithTypeDataExts =
    flip <| List.foldr (uncurry runWithTypeDataExt)



-- BASIC INFERNECE OPERATIONS


applyConstraints : TVarEnv -> Type -> Type
applyConstraints env typ =
    case typ of
        TypeVar a ->
            case lookupTVar env a of
                Just val ->
                    val

                Nothing ->
                    TypeVar a

        BaseType n ->
            BaseType n

        FuncType t1 t2 ->
            FuncType (applyConstraints env t1) (applyConstraints env t2)

        ParamType f typs ->
            ParamType f (List.map (applyConstraints env) typs)


applyConstraintsM : Type -> IE Type
applyConstraintsM typ =
    getEnv |> andThenIE (\env -> return (applyConstraints env typ))


allTVars : Type -> List TVarName
allTVars =
    let
        go : List TVarName -> Type -> List TVarName
        go tvars t =
            case t of
                TypeVar n ->
                    n :: tvars

                BaseType _ ->
                    tvars

                FuncType ta tb ->
                    go tvars ta ++ go tvars tb

                ParamType _ typs ->
                    List.concatMap (go tvars) typs
    in
        go []


rewriteWithFreshTVars : Type -> IE Type
rewriteWithFreshTVars t =
    allTVars t
        |> List.map
            (\n ->
                newTVarM
                    |> andThenIE
                        (\new ->
                            return ( n, new )
                        )
            )
        |> sequenceIEs
        |> andThenIE
            (\subs ->
                return
                    (applyConstraints
                        (TVarEnv
                            (-- [hack] bogus value
                             0
                            )
                            (Dict.fromList subs)
                        )
                        t
                    )
            )


{-| match the typevars in the two type expressions, and
extend the tvar environment with new constraints
-}
setEqual : Type -> Type -> IE Type
setEqual t1 t2 =
    let
        -- Assume type var inputs are free
        go : Type -> Type -> IE Type
        go t1 t2 =
            case ( t1, t2 ) of
                -- Assumes TypeVar n is free
                ( TypeVar n, t ) ->
                    setEqualTVar n t

                -- Assumes TypeVar n is free
                ( t, TypeVar n ) ->
                    setEqualTVar n t

                ( BaseType n1, BaseType n2 ) ->
                    if n1 == n2 then
                        return (BaseType n1)
                    else
                        failCannotSetEqualError t1 t2

                ( FuncType ta1 tb1, FuncType ta2 tb2 ) ->
                    -- [note] by choice of order in map2IE
                    -- this compares arg first
                    -- [q] Should we compare return types first?
                    map2IE (:>) (go ta1 ta2) (go tb1 tb2)

                ( ParamType f1 targs1, ParamType f2 targs2 ) ->
                    if f1 == f2 then
                        List.map2 setEqual targs1 targs2
                            |> sequenceIEs
                            |> mapIE (ParamType f1)
                    else
                        failCannotSetEqualError t1 t2

                _ ->
                    failCannotSetEqualError t1 t2

        setEqualTVar : TVarName -> Type -> IE Type
        setEqualTVar n t =
            case t of
                TypeVar n_ ->
                    if n == n_ then
                        -- leave out trivial reflexivity a = a
                        return (TypeVar n)
                    else
                        envExtendM ( n, TypeVar n_ )
                            |> andThenIE (\() -> return (TypeVar n))

                -- Non-typevar case
                t ->
                    let
                        contains : TVarEnv -> TVarName -> Type -> Bool
                        contains env n t =
                            let
                                go t =
                                    case t of
                                        TypeVar n_ ->
                                            case lookupTVar env n_ of
                                                Just t_ ->
                                                    go t_

                                                Nothing ->
                                                    n == n_

                                        BaseType n_ ->
                                            False

                                        FuncType ta tb ->
                                            go ta || go tb

                                        ParamType _ targs ->
                                            List.any go targs
                            in
                                go t

                        containsM : TVarName -> Type -> IE Bool
                        containsM n t =
                            getEnv
                                |> andThenIE (\env -> return (contains env n t))
                    in
                        containsM n t
                            |> andThenIE
                                (\cont ->
                                    if cont then
                                        getEnv
                                            |> andThenIE
                                                (\env ->
                                                    Debug.crash <|
                                                        Helper.unwords
                                                            [ "Trying to set"
                                                            , showType (TypeVar n)
                                                            , "to"
                                                            , showType t
                                                            , ", but the latter"
                                                            , "contains the former"
                                                            , "in env"
                                                            , showEnv env
                                                            ]
                                                )
                                    else
                                        {- [note] important to subsitute first
                                           Otherwise we might end up with
                                           constraints like a=list a
                                        -}
                                        applyConstraintsM t
                                            |> andThenIE
                                                (\t_ ->
                                                    envExtendM ( n, t_ )
                                                        |> andThenIE
                                                            (\() ->
                                                                return (TypeVar n)
                                                            )
                                                )
                                )
    in
        \tdata env ->
            let
                getFirstNontrivial : Type -> Type
                getFirstNontrivial t =
                    case t of
                        TypeVar n ->
                            case lookupTVar env n of
                                Just t ->
                                    getFirstNontrivial t

                                Nothing ->
                                    t

                        _ ->
                            t
            in
                go (getFirstNontrivial t1) (getFirstNontrivial t2) tdata env
                    |> Result.map
                        (\( t, env1 ) ->
                            -- Helper.log
                            --     [ "setting:"
                            --     , showType t1
                            --     , "\nequal to:"
                            --     , showType t2
                            --     , "\nin env:"
                            --     , showEnv env
                            --     , "\nresulting in:"
                            --     , showEnv env1
                            --     ]
                            ( t, env1 )
                        )


{-| run setEqual, starting from the left, return the last type
-}
setEqualAll : Nonempty Type -> IE Type
setEqualAll =
    Nonempty.map return
        >> Nonempty.foldl1 (\m2 m1 -> map2IE setEqual m1 m2 |> batchIE)



-- MAIN INFERENCE


inferFull : TypeData -> Expr -> Result InferenceError Type
inferFull tData expr =
    infer expr tData emptyEnv
        |> Result.map (uncurry (flip applyConstraints))


inferA : Expr -> IE (ExprA Type)
inferA =
    let
        handleApp f args =
            lookupFreshTypeData f
                |> andThenIE
                    (\t ->
                        let
                            ( returnType, _ ) =
                                unrollFuncTypes t
                        in
                            Nonempty
                                (return returnType)
                                (List.reverse args)
                                |> sequenceIEsNonempty
                                |> mapIE rollToFuncTypes
                                |> andThenIE (setEqual t)
                                |> andThenIE (\_ -> return returnType)
                    )

        var n =
            lookupTypeData n

        hole n =
            newTVarM

        app =
            handleApp

        lit _ =
            return int

        constructor =
            handleApp

        caseStmt e cases =
            cases
                |> sequenceIEsNonempty
                |> andThenIE
                    (\cs ->
                        let
                            ( patterns, rhss ) =
                                Nonempty.unzip cs
                        in
                            -- first match the matched expr with the patterns
                            e
                                |> mapIE (flip (:::) patterns)
                                |> andThenIE setEqualAll
                                |> andThenIE
                                    (-- discard the type,
                                     -- as long as their types all match
                                     \_ ->
                                        -- next match the rhss, returning the (equal) type
                                        rhss |> setEqualAll
                                    )
                    )

        cb c params rhs =
            lookupFreshTypeData c
                |> andThenIE
                    (\t ->
                        let
                            ( returnType, paramTypes ) =
                                unrollFuncTypes t
                        in
                            map2IE ((,))
                                (return returnType)
                                (rhs
                                    |> runWithTypeDataExts
                                        (List.map2 ((,)) params paramTypes)
                                )
                    )
    in
        Expr.scanr var hole app lit constructor caseStmt cb
            >> sequenceIEsExprA


infer : Expr -> IE Type
infer =
    let
        handleApp f args =
            lookupFreshTypeData f
                |> andThenIE
                    (\t ->
                        let
                            ( returnType, _ ) =
                                unrollFuncTypes t
                        in
                            Nonempty
                                (return returnType)
                                (List.reverse args)
                                |> sequenceIEsNonempty
                                |> mapIE rollToFuncTypes
                                |> andThenIE (setEqual t)
                                |> andThenIE (\_ -> return returnType)
                    )

        var n =
            lookupTypeData n

        hole n =
            newTVarM

        app =
            handleApp

        lit _ =
            return int

        constructor =
            handleApp

        caseStmt e cases =
            cases
                |> sequenceIEsNonempty
                |> andThenIE
                    (\cs ->
                        let
                            ( patterns, rhss ) =
                                Nonempty.unzip cs
                        in
                            -- first match the matched expr with the patterns
                            e
                                |> mapIE (flip (:::) patterns)
                                |> andThenIE setEqualAll
                                |> andThenIE
                                    (-- discard the type,
                                     -- as long as their types all match
                                     \_ ->
                                        -- next match the rhss, returning the (equal) type
                                        rhss |> setEqualAll
                                    )
                    )

        cb c params rhs =
            lookupFreshTypeData c
                |> andThenIE
                    (\t ->
                        let
                            ( returnType, paramTypes ) =
                                unrollFuncTypes t
                        in
                            map2IE ((,))
                                (return returnType)
                                (rhs
                                    |> runWithTypeDataExts
                                        (List.map2 ((,)) params paramTypes)
                                )
                    )
    in
        Expr.foldr var hole app lit constructor caseStmt cb



-- PRINTING


showType : Type -> String
showType t =
    case t of
        TypeVar n ->
            "t" ++ toString n

        BaseType n ->
            n

        ParamType f args ->
            Helper.bracket <| Helper.unwords (f :: List.map showType args)

        FuncType t1 t2 ->
            Helper.bracket <| Helper.unwords [ showType t1, "->", showType t2 ]


showEnv : TVarEnv -> String
showEnv (TVarEnv n dict) =
    Helper.unwords
        [ "{"
        , dict
            |> Dict.map (\n t -> showType (TypeVar n) ++ ":" ++ showType t)
            |> Dict.values
            |> List.intersperse ","
            |> Helper.unwords
        , "}"
        ]


showTypeData : TypeData -> String
showTypeData tdata =
    Helper.unlines
        [ "{"
        , tdata
            |> Dict.map (\n t -> n ++ ":" ++ showType t)
            |> Dict.values
            |> Helper.unlines
        , "}"
        ]


showErr : InferenceError -> String
showErr err =
    case err of
        CannotSetEqualError t1 t2 env ->
            Helper.unwords
                [ "Cannot match type:"
                , showType t1
                , "with type:"
                , showType t2
                , ". The bindings are:"
                , showEnv env
                , "."
                ]

        MissingTypeInfo varname ->
            Helper.unwords
                [ "Missing type info for variable:"
                , varname
                , "."
                ]



-- HELPERS


unrollFuncTypes : Type -> ( Type, List Type )
unrollFuncTypes =
    Helper.generateWrite
        (\t ->
            case t of
                FuncType a b ->
                    ( Just b, a )

                t_ ->
                    ( Nothing, t_ )
        )
        -- [note] currently in the form of:
        -- paramType1 -> paramType2 -> paramType3 -> returnType
        >> Nonempty.reverse
        >> (\(Nonempty returnType reversedParams) ->
                ( returnType, List.reverse reversedParams )
           )


rollToFuncTypes : Nonempty Type -> Type
rollToFuncTypes =
    Nonempty.foldl1 (:>)
