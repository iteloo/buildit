module TypeInferencer exposing (..)

import Dict
import Block exposing (Expr(..))
import Html exposing (text)


main =
    text <|
        toString <|
            infer typeData addHole Nothing


addHole : Expr
addHole =
    App addId [ Lit 1, Hole "y" ]


type alias Name =
    String


type BlockType
    = BlockType Type (List Type)


type Type
    = TypeVar Name
    | BaseType Name
    | ParamType Name (List Type)


type alias TypeData =
    Dict.Dict Name BlockType


typeData : TypeData
typeData =
    Dict.fromList
        [ ( addId, BlockType int [ int, int ] )
        , ( reverseId, BlockType (list a) [ list a ] )
        ]


type alias Constraint =
    ( Name, Type )


type InferenceError
    = NoMatch


infer :
    TypeData
    -> Expr
    -> Maybe Type
    -> Result InferenceError ( Type, List Constraint )
infer data =
    let
        applyConstraints : List Constraint -> Type -> Type
        applyConstraints cts typ =
            case typ of
                TypeVar a ->
                    case Dict.get a (Dict.fromList cts) of
                        Just val ->
                            val

                        Nothing ->
                            TypeVar a

                BaseType n ->
                    BaseType n

                ParamType f typs ->
                    ParamType f (List.map (applyConstraints cts) typs)

        -- matches vars in the pattern with type subexpr in typ
        -- returns Nothing if cannot match
        match : Type -> Type -> Maybe (List Constraint)
        match pattern typ =
            case ( pattern, typ ) of
                ( TypeVar a, t ) ->
                    Just [ ( a, t ) ]

                ( BaseType p, BaseType t ) ->
                    if p == t then
                        Just []
                    else
                        Nothing

                ( ParamType fp ps, ParamType ft ts ) ->
                    if fp == ft then
                        List.foldl
                            (\( p, t ) mcts ->
                                mcts
                                    |> Maybe.andThen
                                        (\cts ->
                                            Maybe.map ((++) cts)
                                                (match (applyConstraints cts p) t)
                                        )
                            )
                            (Just [])
                        <|
                            List.map2 ((,)) ps ts
                    else
                        Nothing

                _ ->
                    Nothing

        var :
            Name
            -> Maybe Type
            -> Result InferenceError ( Type, List Constraint )
        var n mc =
            case mc of
                Just c ->
                    Ok ( c, [] )

                Nothing ->
                    Ok ( TypeVar "a", [] )

        hole n mc =
            case mc of
                Just c ->
                    Ok ( c, [] )

                Nothing ->
                    Ok ( TypeVar "a", [] )

        app f args mc =
            {- Example: List.Singleton : b -> List b
               1. We match the type passed down with the
                 return type of the definition, writing the vars
                 in the def in terms of the types in the lhs

                 ex.  Form Constraint => List (List a)  (ALWAYS more general)
                      Def => List b    (after alpha conversion)
                      Constraints = [b = List a]

                2. Starting with the first parameter, we apply all
                constrainsts so far (just b=List a in our example)
                to its type, and match it to the type of the first
                argument.

                ex.  1st Param => b
                     Constrains = [ b = List a ]
                     type of 1st Param => List a

                3. We pass this down the recursion through the arguments.
                4. Match the type of the argument with the param type,
                writing the vars in the param type in terms of the type
                of the arg (which is always less general)
            -}
            case Dict.get f data of
                Just (BlockType returnType paramTypes) ->
                    List.foldl
                        (\( arg, paramType ) ->
                            Result.andThen
                                (\cts ->
                                    arg (Just (applyConstraints cts paramType))
                                        |> Result.map
                                            (Tuple.second >> (++) cts)
                                )
                        )
                        (Ok [])
                        (List.map2 ((,)) args paramTypes)
                        |> Result.andThen
                            (flip applyConstraints returnType
                                -- [note] cts aren't useful anymore
                                -- after this, since all variables
                                -- are eliminated
                                >> (\t ->
                                        case mc of
                                            Just c ->
                                                match t c
                                                    |> Result.fromMaybe NoMatch
                                                    |> Result.map
                                                        (\cts -> ( t, cts ))

                                            Nothing ->
                                                Ok ( t, [] )
                                   )
                            )

                Nothing ->
                    Debug.crash "No type data found"

        constructor _ =
            Debug.crash ""

        caseStmt _ =
            Debug.crash ""

        cb _ =
            Debug.crash ""

        lit _ mc =
            case mc of
                Just c ->
                    if c == int then
                        Ok ( c, [] )
                    else
                        Err NoMatch

                Nothing ->
                    Ok ( int, [] )
    in
        Block.foldr var hole app lit constructor caseStmt cb



-- HELPERS


addId =
    "add"


reverseId =
    "reverse"


int : Type
int =
    BaseType "int"


list : Type -> Type
list =
    ParamType "list" << List.singleton


a =
    TypeVar "a"


b =
    TypeVar "b"
