module TypeInfer exposing (..)

import Expr exposing (..)
import Helper
import Dict
import Set
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


scanrExprAIEs var hole app lit constructor caseStmt caseBranch =
    toExprA
        >> scanrA
            (always var)
            (always hole)
            (always app)
            (always lit)
            (always constructor)
            (always caseStmt)
            caseBranch


scanrAExprAIEs :
    (a -> Name -> IE s)
    -> (a -> Name -> IE s)
    -> (a -> Name -> List s -> IE s)
    -> (a -> Int -> IE s)
    -> (a -> Name -> List s -> IE s)
    -> (a -> s -> Nonempty t -> IE s)
    -> (Name -> List Name -> s -> IE t)
    -> ExprA a
    -> IE (ExprA s)
scanrAExprAIEs var hole app lit constructor caseStmt caseBranch =
    foldrA
        (\a n ->
            var a n
                |> andThenIE
                    (\acc ->
                        return
                            ( VarA acc n
                            , acc
                            )
                    )
        )
        (\a n ->
            hole a n
                |> andThenIE
                    (\acc ->
                        return
                            ( HoleA acc n
                            , acc
                            )
                    )
        )
        (\a f sargs ->
            sargs
                |> sequenceIEs
                |> andThenIE
                    (\args ->
                        let
                            ( argsVal, argsAcc ) =
                                List.unzip args
                        in
                            app a f argsAcc
                                |> andThenIE
                                    (\acc ->
                                        return
                                            ( AppA acc f argsVal
                                            , acc
                                            )
                                    )
                    )
        )
        (\a x ->
            lit a x
                |> andThenIE
                    (\acc ->
                        return
                            ( LitA acc x
                            , acc
                            )
                    )
        )
        (\a c sargs ->
            sargs
                |> sequenceIEs
                |> andThenIE
                    (\args ->
                        let
                            ( argsVal, argsAcc ) =
                                List.unzip args
                        in
                            constructor a c argsAcc
                                |> andThenIE
                                    (\acc ->
                                        return
                                            ( ConstructorA acc c argsVal
                                            , acc
                                            )
                                    )
                    )
        )
        (\a se scases ->
            scases
                |> sequenceIEsNonempty
                |> andThenIE
                    (\cases ->
                        let
                            ( casesVal, casesAcc ) =
                                Nonempty.unzip cases
                        in
                            se
                                |> andThenIE
                                    (\( e, eAcc ) ->
                                        caseStmt a eAcc casesAcc
                                            |> andThenIE
                                                (\acc ->
                                                    return
                                                        ( CaseStmtA acc e casesVal
                                                        , acc
                                                        )
                                                )
                                    )
                    )
        )
        (\c params srhs ->
            srhs
                |> andThenIE
                    (\( rhs, rhsAcc ) ->
                        caseBranch c params rhsAcc
                            |> andThenIE
                                (\acc ->
                                    return
                                        ( CaseA c params rhs
                                        , acc
                                        )
                                )
                    )
        )
        >> mapIE Tuple.first



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



-- BASIC INFERENCE OPERATIONS


applyConstraints : TVarEnv -> Type -> Type
applyConstraints env typ =
    let
        go =
            applyConstraints env
    in
        case typ of
            TypeVar a ->
                case lookupTVar env a of
                    Just val ->
                        go val

                    Nothing ->
                        TypeVar a

            BaseType n ->
                BaseType n

            FuncType t1 t2 ->
                FuncType (go t1) (go t2)

            ParamType f typs ->
                ParamType f (List.map go typs)


applyConstraintsM : Type -> IE Type
applyConstraintsM typ =
    getEnv |> andThenIE (\env -> return (applyConstraints env typ))


allTVars : Type -> Set.Set TVarName
allTVars =
    let
        go : Set.Set TVarName -> Type -> Set.Set TVarName
        go tvars t =
            case t of
                TypeVar n ->
                    Set.insert n tvars

                BaseType _ ->
                    tvars

                FuncType ta tb ->
                    Set.union (go tvars ta) (go tvars tb)

                ParamType _ typs ->
                    List.map (go tvars) typs
                        |> List.foldr Set.union Set.empty
    in
        go Set.empty


rewriteWithFreshTVars : Type -> IE Type
rewriteWithFreshTVars t =
    let
        rename : Dict.Dict TVarName Type -> Type -> Type
        rename sub typ =
            let
                go =
                    rename sub
            in
                case typ of
                    TypeVar n ->
                        case Dict.get n sub of
                            Just val ->
                                val

                            Nothing ->
                                TypeVar n

                    BaseType n ->
                        BaseType n

                    FuncType t1 t2 ->
                        FuncType (go t1) (go t2)

                    ParamType f typs ->
                        ParamType f (List.map go typs)
    in
        allTVars t
            |> Set.toList
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
                    return (rename (Dict.fromList subs) t)
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
                ( t, TypeVar n ) ->
                    setEqualTVar n t

                -- Assumes TypeVar n is free
                ( TypeVar n, t ) ->
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
inferFull tdata expr =
    inferAFull tdata expr |> Result.map getA


infer : Expr -> IE Type
infer =
    inferA >> mapIE getA


inferAFull : TypeData -> Expr -> Result InferenceError (ExprA Type)
inferAFull tdata expr =
    inferA expr tdata emptyEnv
        |> Result.map (\( exprA, env ) -> mapA (applyConstraints env) exprA)


inferA : Expr -> IE (ExprA Type)
inferA =
    let
        handleApp constr f args =
            lookupFreshTypeData f
                |> andThenIE
                    (\t ->
                        let
                            ( returnType, _ ) =
                                unrollFuncTypes t
                        in
                            args
                                |> sequenceIEs
                                |> andThenIE
                                    (\argExprs ->
                                        let
                                            argTypes =
                                                List.map getA argExprs
                                        in
                                            rollToFuncTypes returnType argTypes
                                                |> setEqual t
                                                |> andThenIE
                                                    (\_ ->
                                                        return (constr returnType f argExprs)
                                                    )
                                    )
                    )

        var : Name -> IE (ExprA Type)
        var n =
            lookupTypeData n
                |> andThenIE (\t -> return (VarA t n))

        hole n =
            newTVarM
                |> andThenIE (\t -> return (HoleA t n))

        app =
            handleApp AppA

        lit x =
            return (LitA int x)

        constructor =
            handleApp ConstructorA

        caseStmt :
            IE (ExprA Type)
            ->
                Nonempty
                    (IE
                        { caseExpr : CaseA Type
                        , lhsType : Type
                        , rhsType : Type
                        }
                    )
            -> IE (ExprA Type)
        caseStmt me cases =
            cases
                -- [note] must sequence the tuples together
                -- (in the same monadic context)
                |> sequenceIEsNonempty
                |> andThenIE
                    (\cs ->
                        let
                            caseExprs : Nonempty (CaseA Type)
                            caseExprs =
                                Nonempty.map .caseExpr cs

                            lhsTypes : Nonempty Type
                            lhsTypes =
                                Nonempty.map .lhsType cs

                            rhsTypes : Nonempty Type
                            rhsTypes =
                                Nonempty.map .rhsType cs
                        in
                            -- first match the matched expr with the patterns
                            me
                                |> andThenIE
                                    (\e ->
                                        (getA e ::: lhsTypes)
                                            |> setEqualAll
                                            |> andThenIE
                                                (-- discard the type,
                                                 -- as long as their types all match
                                                 \_ ->
                                                    -- next match the rhss, returning the (equal) type
                                                    rhsTypes
                                                        |> setEqualAll
                                                        |> andThenIE
                                                            (\rhsType ->
                                                                return (CaseStmtA rhsType e caseExprs)
                                                            )
                                                )
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
                            rhs
                                |> runWithTypeDataExts
                                    (List.map2 ((,)) params paramTypes)
                                |> andThenIE
                                    (\rhsA ->
                                        return
                                            { caseExpr = CaseA c params rhsA
                                            , lhsType = returnType
                                            , rhsType = getA rhsA
                                            }
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
            |> Dict.map (\n t -> showType (TypeVar n) ++ "->" ++ showType t)
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


rollToFuncTypes : Type -> List Type -> Type
rollToFuncTypes returnType paramTypes =
    Nonempty returnType (List.reverse paramTypes)
        |> Nonempty.foldl1 (:>)
