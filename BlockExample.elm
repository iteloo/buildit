module BlockExample exposing (..)

import Block exposing (..)
import TypeInfer exposing (..)
import Helper
import Dict
import List.Nonempty exposing (Nonempty(..))


-- TYPE VARS


a =
    TypeVar 1


b =
    TypeVar 2



-- ARITHMETICS


addId =
    "add"


add x y =
    App addId [ x, y ]



-- LISTs


list : Type -> Type
list =
    ParamType "list" << List.singleton


emptyListId =
    "[]"


emptyList : Expr
emptyList =
    Constructor emptyListId []


consId =
    "Cons"


cons : Expr -> Expr -> Expr
cons x y =
    Constructor consId [ x, y ]


listCase : Expr -> Expr -> (Expr -> Expr -> Expr) -> Expr
listCase e ifEmpty ifCons =
    CaseStmt e <|
        Nonempty
            (Case emptyListId [] ifEmpty)
            [ let
                head =
                    "head"

                rest =
                    "rest"
              in
                Case consId
                    [ head, rest ]
                    (ifCons (Var head) (Var rest))
            ]


singleton a =
    cons a emptyList


appendId =
    "append"


append xs ys =
    App appendId [ xs, ys ]


reverseId =
    "reverse"



-- EITHER


either : Type -> Type -> Type
either a b =
    ParamType "either" [ a, b ]



-- EXAMPLE TYPE DATA


typeData : TypeData
typeData =
    Dict.fromList
        [ ( addId, int :> int :> int )
        , ( consId, a :> list a :> list a )
        , ( emptyListId, list a )
        , ( appendId, list a :> list a :> list a )
        , ( reverseId, list a :> list a )
        , ( "map", mapType )
        ]


mapType =
    (a :> b) :> list a :> list b


intMapType =
    (int :> b) :> list int :> list b



-- EXAMPLE DEFS


defs =
    Dict.fromList
        [ ( addId, addDef )
        , ( "add1", add1Def )
        , ( appendId, appendDef )
        ]


intId =
    "int"


listId tstr =
    Helper.unwords [ "list of", tstr ]


addDef =
    Def
        (DefLhs intId
            [ DefVar intId "x"
            , DefText "+"
            , DefVar intId "y"
            ]
        )
        -- [hack] bogus value
        (Var "x")


add1Def =
    Def
        (DefLhs intId
            [ DefVar intId "x"
            , DefText "incremented by 1"
            ]
        )
        (add (Var "x") (Lit 1))


appendDef =
    let
        aList =
            "a list"

        anotherList =
            "another list"

        listofInt =
            listId intId
    in
        Def
            (DefLhs listofInt
                [ DefVar listofInt aList
                , DefText "followed by"
                , DefVar listofInt anotherList
                ]
            )
            (listCase (Var aList)
                (Var anotherList)
                (\x xs -> (cons x (append xs (Var anotherList))))
            )



-- EXAMPLE EXPR


listExprFromListOfInt : List Int -> Expr
listExprFromListOfInt =
    List.foldr cons emptyList << List.map Lit


range02 : Expr
range02 =
    listExprFromListOfInt (List.range 0 2)


range56 =
    listExprFromListOfInt (List.range 5 6)


addHole : Expr
addHole =
    add (Lit 1) (Hole "y")


add23 : Expr
add23 =
    add (Lit 2) (Lit 3)
