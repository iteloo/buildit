module TypeInfer exposing (..)

import Block exposing (Expr(..))
import Dict
import Html exposing (text)
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


type TVarEnv
    = TVarEnv Int (Dict.Dict Int Type)


type alias TypeData =
    Dict.Dict Name Type


type InferenceError
    = NoMatch


type alias ModEnv a =
    TVarEnv -> Result InferenceError ( a, TVarEnv )


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
    \env -> Ok ( Debug.log (showEnv env) (), env )


sequenceModEnvs : List (ModEnv a) -> ModEnv (List a)
sequenceModEnvs =
    List.foldr (map2ModEnv (::)) (return [])


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
                        fail NoMatch

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
                        fail NoMatch

                _ ->
                    fail NoMatch

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
                                                unwords
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
                            Debug.log
                                (unwords
                                    [ "setting:"
                                    , showType t1
                                    , "\nequal to:"
                                    , showType t2
                                    , "\nin env:"
                                    , showEnv env
                                    , "\nresulting in:"
                                    , showEnv env1
                                    ]
                                )
                                ()
                                |> (\() -> ( t, env1 ))
                        )


{-| run setEqual, starting from the left, return the last type
-}
setEqualAll : Nonempty Type -> ModEnv Type
setEqualAll =
    Nonempty.map return
        >> Nonempty.foldl1
            (\m1 m2 ->
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
        unrollFuncTypes : Type -> Nonempty Type
        unrollFuncTypes =
            Nonempty.reverse
                << generateWrite
                    (\t ->
                        case t of
                            FuncType a b ->
                                ( Just b, a )

                            t_ ->
                                ( Nothing, t_ )
                    )

        rollToFuncTypes : Nonempty Type -> Type
        rollToFuncTypes =
            Nonempty.foldl1 (:>)

        handleFunc f args env =
            case Dict.get f env of
                Just t_ ->
                    rewriteWithFreshTVars t_
                        |> andThenModEnv
                            (\t ->
                                let
                                    ( returnType, _ ) =
                                        case unrollFuncTypes t of
                                            Nonempty x xs ->
                                                ( x, xs )
                                in
                                    -- lookup type signature for f
                                    -- create a new return tv for each arg
                                    -- match whole thing with type of f
                                    Nonempty
                                        (return returnType)
                                        (List.map ((|>) env) <| List.reverse args)
                                        |> Nonempty.toList
                                        |> sequenceModEnvs
                                        |> mapModEnv (toNonemptyUnsafe >> rollToFuncTypes)
                                        |> andThenModEnv
                                            (\x ->
                                                unsafePrintEnv
                                                    |> andThenModEnv (\_ -> return x)
                                            )
                                        |> andThenModEnv (setEqual t)
                                        |> andThenModEnv (\_ -> unsafePrintEnv)
                                        |> andThenModEnv (\_ -> return returnType)
                            )

                Nothing ->
                    Debug.crash <|
                        unwords [ "Cannot find type info for", f ]

        var n env =
            case Dict.get n env of
                Just t ->
                    return t

                Nothing ->
                    Debug.crash <|
                        unwords [ "Cannot find type info for", n ]

        hole n _ =
            newTVarM

        app =
            handleFunc

        lit _ env =
            return int

        constructor =
            handleFunc

        caseStmt e cases env =
            let
                cs =
                    cases
                        |> List.map ((|>) env)

                -- [tofix] handle (error) case where there's no patterns!
                -- probably change to Nonempty
                patterns =
                    cs |> List.map (mapModEnv Tuple.first)

                rhss =
                    cs |> List.map (mapModEnv Tuple.second)
            in
                Nonempty (e env) patterns
                    |> Nonempty.toList
                    |> sequenceModEnvs
                    |> andThenModEnv (toNonemptyUnsafe >> setEqualAll)
                    |> andThenModEnv
                        (-- discard the type, as long as their types all match
                         \_ ->
                            -- now match the rhs, returning the (equal) type
                            rhss
                                |> sequenceModEnvs
                                |> andThenModEnv (toNonemptyUnsafe >> setEqualAll)
                        )

        cb c params rhs env =
            case Dict.get c env of
                Just t ->
                    let
                        ( returnType, paramTypes ) =
                            case unrollFuncTypes t of
                                Nonempty x xs ->
                                    ( x, xs )

                        extendedEnv =
                            List.map2 ((,)) params paramTypes
                                |> List.foldr (uncurry Dict.insert) env
                    in
                        map2ModEnv ((,)) (return t) (rhs extendedEnv)

                Nothing ->
                    Debug.crash <|
                        unwords [ "Cannot find type info for", c ]
    in
        flip <| Block.foldr var hole app lit constructor caseStmt cb



-- EXEC


int : Type
int =
    BaseType "int"


list : Type -> Type
list =
    ParamType "list" << List.singleton


either : Type -> Type -> Type
either a b =
    ParamType "either" [ a, b ]


a =
    TypeVar 1


b =
    TypeVar 2


mapType =
    FuncType (FuncType a b) (FuncType (list a) (list b))


intMapType =
    FuncType (FuncType int b) (FuncType (list int) (list b))


infixr 9 :>
(:>) =
    FuncType


unwords =
    List.foldr (++) "" << List.intersperse " "


bracket x =
    "(" ++ x ++ ")"


showType : Type -> String
showType t =
    case t of
        TypeVar n ->
            "t" ++ toString n

        BaseType n ->
            n

        ParamType f args ->
            bracket <| unwords (f :: List.map showType args)

        FuncType t1 t2 ->
            bracket <| unwords [ showType t1, "->", showType t2 ]


ex1 =
    setEqual mapType intMapType emptyEnv


ex2 =
    setEqual a b emptyEnv


ex3 =
    setEqual int b emptyEnv


ex4 =
    setEqual (list int) (list a) emptyEnv


ex5 =
    setEqual (int :> a) (b :> a) emptyEnv


{-| should give no match
-}
ex6 =
    setEqual (either a a) (either int (list int)) emptyEnv


ex7 =
    add23


ex8 =
    Var addId


ex9 =
    Block.range02


ex10 =
    append Block.range02 Block.range56


ex11 =
    append Block.range56 ex10


ex12 =
    singleton (singleton (Lit 1))


ex13 =
    append
        (singleton Block.range56)
        (append
            (singleton Block.range02)
            (singleton Block.emptyList)
        )


ex14 =
    append
        Block.emptyList
        (singleton Block.emptyList)


ex15 =
    append
        (singleton Block.range56)
        (append
            (singleton Block.range02)
            (singleton Block.emptyList)
        )


ex16 =
    listCase Block.emptyList
        (Lit 1)
        (\_ _ -> Lit 1)


ex =
    ex16


showEnv : TVarEnv -> String
showEnv (TVarEnv n dict) =
    unwords
        [ "{"
        , dict
            |> Dict.map (\n t -> showType (TypeVar n) ++ ":" ++ showType t)
            |> Dict.values
            |> List.foldr (++) ""
        , "}"
        ]


main =
    text <|
        case inferComplete typeData ex of
            Ok t ->
                unwords [ showType t ]

            Err e ->
                toString e


append xs ys =
    App Block.appendId [ xs, ys ]


listCase : Expr -> Expr -> (Expr -> Expr -> Expr) -> Expr
listCase e ifEmpty ifCons =
    CaseStmt e
        [ Block.Case Block.emptyListId [] ifEmpty
        , let
            head =
                "head"

            rest =
                "rest"
          in
            Block.Case Block.consId [ head, rest ] (ifCons (Var head) (Var rest))
        ]


singleton a =
    Block.cons a Block.emptyList


addId =
    "add"


reverseId =
    "reverse"


addHole : Expr
addHole =
    App addId [ Lit 1, Hole "y" ]


add23 : Expr
add23 =
    App addId [ Lit 2, Lit 3 ]


typeData : TypeData
typeData =
    Dict.fromList
        [ ( addId, int :> int :> int )
        , ( reverseId, list a :> list a )
        , ( Block.consId, a :> list a :> list a )
        , ( Block.emptyListId, list a )
        , ( Block.appendId, list a :> list a :> list a )
        , ( "map", mapType )
        ]


generateWrite : (a -> ( Maybe a, b )) -> a -> Nonempty b
generateWrite gen =
    let
        go : List b -> a -> Nonempty b
        go bs a =
            case gen a of
                ( Nothing, b ) ->
                    Nonempty b bs

                ( Just next, b ) ->
                    go (b :: bs) next
    in
        Nonempty.reverse << go []


generate : (a -> Maybe a) -> a -> Nonempty a
generate gen =
    let
        go : List a -> a -> Nonempty a
        go xs x =
            case gen x of
                Nothing ->
                    Nonempty x xs

                Just next ->
                    go (x :: xs) next
    in
        Nonempty.reverse << go []


toNonemptyUnsafe xs =
    case Nonempty.fromList xs of
        Just xs ->
            xs

        Nothing ->
            Debug.crash <|
                unwords
                    [ "Cannot turn empty list"
                    , "into nonempty list"
                    ]
