module TypeInfer exposing (..)

import Block exposing (Expr(..))
import Helper
import Dict
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


type alias Name =
    String


type alias TVarName =
    Int


type Type
    = TypeVar TVarName
    | BaseType Name
    | FuncType Type Type
    | ParamType Name (List Type)


infixr 9 :>
(:>) =
    FuncType


int : Type
int =
    BaseType "int"



-- Inference Environment (ModEnv)


type alias ModEnv a =
    TVarEnv -> Result InferenceError ( a, TVarEnv )


type TVarEnv
    = TVarEnv Int (Dict.Dict Int Type)


type InferenceError
    = CannotSetEqualError Type Type TVarEnv
    | MissingTypeInfo Name


getEnv : ModEnv TVarEnv
getEnv =
    \env -> Ok ( env, env )


newTVarM : ModEnv Type
newTVarM =
    Ok << newTVar


newTVar : TVarEnv -> ( Type, TVarEnv )
newTVar (TVarEnv n d) =
    ( TypeVar n, TVarEnv (n + 1) d )


emptyEnv : TVarEnv
emptyEnv =
    TVarEnv 0 Dict.empty


lookupTVar : TVarEnv -> TVarName -> Maybe Type
lookupTVar (TVarEnv _ d) n =
    Dict.get n d


envExtend : ( TVarName, Type ) -> TVarEnv -> TVarEnv
envExtend ( n, t ) (TVarEnv cur dict) =
    TVarEnv cur (Dict.insert n t dict)


envExtendM : ( TVarName, Type ) -> ModEnv ()
envExtendM binding =
    envExtend binding >> return ()


return : a -> ModEnv a
return =
    curry Ok


fail : InferenceError -> ModEnv a
fail err _ =
    Err err


failCannotSetEqualError : Type -> Type -> ModEnv ()
failCannotSetEqualError t1 t2 =
    getEnv |> andThenModEnv (\env -> fail (CannotSetEqualError t1 t2 env))


andThenModEnv : (a -> ModEnv b) -> ModEnv a -> ModEnv b
andThenModEnv f ma =
    ma >> Result.andThen (uncurry f)


apModEnv : ModEnv (a -> b) -> ModEnv a -> ModEnv b
apModEnv mf ma =
    mf
        |> andThenModEnv
            (\f ->
                ma |> andThenModEnv (f >> return)
            )


mapModEnv : (a -> b) -> ModEnv a -> ModEnv b
mapModEnv f =
    andThenModEnv (return << f)


map2ModEnv : (a -> b -> c) -> ModEnv a -> ModEnv b -> ModEnv c
map2ModEnv f =
    apModEnv << mapModEnv f


unsafePrintEnv : ModEnv ()
unsafePrintEnv =
    \env -> Ok ( Helper.log [ showEnv env ] (), env )


sequenceModEnvs : List (ModEnv a) -> ModEnv (List a)
sequenceModEnvs =
    List.foldr (map2ModEnv (::)) (return [])


sequenceModEnvsNonempty : Nonempty (ModEnv a) -> ModEnv (Nonempty a)
sequenceModEnvsNonempty =
    Helper.nonemptyFoldr (map2ModEnv (:::)) (mapModEnv Nonempty.fromElement)



-- VARIABLE BINDING


type alias TypeData =
    Dict.Dict Name Type



-- BASIC INFERNECE OPERATIONS


lookupFreshTypeData : TypeData -> Name -> ModEnv Type
lookupFreshTypeData tdata varname =
    case Dict.get varname tdata of
        Just t ->
            rewriteWithFreshTVars t

        Nothing ->
            fail (MissingTypeInfo varname)


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


applyConstraintsM : Type -> ModEnv Type
applyConstraintsM typ =
    \env -> Ok ( applyConstraints env typ, env )


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


rewriteWithFreshTVars : Type -> ModEnv Type
rewriteWithFreshTVars t =
    allTVars t
        |> List.map
            (\n ->
                newTVarM
                    |> andThenModEnv
                        (\new ->
                            return ( n, new )
                        )
            )
        |> sequenceModEnvs
        |> andThenModEnv
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
setEqual : Type -> Type -> ModEnv ()
setEqual t1 t2 =
    let
        -- Assume type var inputs are free
        go : Type -> Type -> ModEnv ()
        go t1 t2 =
            case ( t1, t2 ) of
                -- Assumes TypeVar n is free
                ( TypeVar n, t ) ->
                    setEqualTVar n t

                -- Assumes TypeVar n is free
                ( t, TypeVar n ) ->
                    setEqualTVar n t

                ( BaseType p, BaseType t ) ->
                    if p == t then
                        return ()
                    else
                        failCannotSetEqualError t1 t2

                ( FuncType ta1 tb1, FuncType ta2 tb2 ) ->
                    -- compare return types first
                    go tb1 tb2
                        |> andThenModEnv
                            (\_ ->
                                go ta1 ta2
                            )

                ( ParamType f1 targs1, ParamType f2 targs2 ) ->
                    if f1 == f2 then
                        mapModEnv (always ()) <|
                            sequenceModEnvs <|
                                List.map2 setEqual targs1 targs2
                    else
                        failCannotSetEqualError t1 t2

                _ ->
                    failCannotSetEqualError t1 t2

        setEqualTVar : TVarName -> Type -> ModEnv ()
        setEqualTVar n t =
            case t of
                TypeVar n_ ->
                    if n == n_ then
                        -- leave out trivial reflexivity a = a
                        return ()
                    else
                        envExtendM ( n, TypeVar n_ )

                -- Non-typevar case
                t_ ->
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

                        containsM : TVarName -> Type -> ModEnv Bool
                        containsM n t =
                            \env -> Ok ( contains env n t, env )
                    in
                        containsM n t_
                            |> andThenModEnv
                                (\cont ->
                                    \env ->
                                        if cont then
                                            Debug.crash <|
                                                Helper.unwords
                                                    [ "Trying to set"
                                                    , showType (TypeVar n)
                                                    , "to"
                                                    , showType t_
                                                    , ", but the latter"
                                                    , "contains the former"
                                                    , "in env"
                                                    , showEnv env
                                                    ]
                                        else
                                            {- [note] important to subsitute first
                                               Otherwise we might end up with
                                               constraints like a=list a
                                            -}
                                            (applyConstraintsM t_
                                                |> andThenModEnv
                                                    (\t__ ->
                                                        envExtendM ( n, t__ )
                                                    )
                                            )
                                                env
                                )
    in
        \env ->
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
                go (getFirstNontrivial t1) (getFirstNontrivial t2) env
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
setEqualAll : Nonempty Type -> ModEnv Type
setEqualAll =
    Nonempty.map return
        >> Nonempty.foldl1
            (\m2 m1 ->
                m1
                    |> andThenModEnv
                        (\t1 ->
                            m2
                                |> andThenModEnv
                                    (\t2 ->
                                        setEqual t1 t2
                                            |> andThenModEnv
                                                (\_ ->
                                                    return t2
                                                )
                                    )
                        )
            )


inferComplete : TypeData -> Expr -> Result InferenceError Type
inferComplete tData expr =
    infer tData expr emptyEnv
        |> Result.map (uncurry (flip applyConstraints))


infer : TypeData -> Expr -> ModEnv Type
infer =
    let
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

        handleFunc f args env =
            lookupFreshTypeData env f
                |> andThenModEnv
                    (\t_ ->
                        rewriteWithFreshTVars t_
                            |> andThenModEnv
                                (\t ->
                                    let
                                        ( returnType, _ ) =
                                            unrollFuncTypes t
                                    in
                                        -- lookup type signature for f
                                        -- create a new return tv for each arg
                                        -- match whole thing with type of f
                                        Nonempty
                                            (return returnType)
                                            (List.map ((|>) env) <| List.reverse args)
                                            |> sequenceModEnvsNonempty
                                            |> mapModEnv rollToFuncTypes
                                            |> andThenModEnv (setEqual t)
                                            |> andThenModEnv (\_ -> return returnType)
                                )
                    )

        var n env =
            case Dict.get n env of
                Just t ->
                    return t

                Nothing ->
                    Debug.crash <|
                        Helper.unwords [ "Cannot find type info for", n ]

        hole n _ =
            newTVarM

        app =
            handleFunc

        lit _ env =
            return int

        constructor =
            handleFunc

        caseStmt e cases env =
            cases
                |> Nonempty.map ((|>) env)
                |> sequenceModEnvsNonempty
                |> andThenModEnv
                    (\cs ->
                        let
                            ( patterns, rhss ) =
                                Nonempty.unzip cs
                        in
                            e env
                                |> mapModEnv (flip (:::) patterns)
                                |> andThenModEnv setEqualAll
                                |> andThenModEnv
                                    (-- discard the type, as long as their types all match
                                     \_ ->
                                        -- now match the rhs, returning the (equal) type
                                        rhss |> setEqualAll
                                    )
                    )

        cb c params rhs env =
            lookupFreshTypeData env c
                |> andThenModEnv
                    (\t ->
                        let
                            ( returnType, paramTypes ) =
                                unrollFuncTypes t

                            extendedEnv =
                                List.map2 ((,)) params paramTypes
                                    |> List.foldr (uncurry Dict.insert) env
                        in
                            map2ModEnv ((,))
                                (return returnType)
                                (rhs extendedEnv
                                 -- |> Helper.log
                                 --     [ "in cb:"
                                 --     , showTypeData extendedEnv
                                 --     ]
                                )
                    )
    in
        flip <| Block.foldr var hole app lit constructor caseStmt cb



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
