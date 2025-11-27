module ItemForm exposing (..)


type FieldMode
    = ViewMode
    | EditMode


type FieldName
    = Name (Maybe String)
    | QCount (Maybe Float)
    | QUnit (Maybe String)
    | Comment (Maybe String)
    | Symbol (Maybe String)


type ItemField
    = ItemField FieldName FieldMode


fields : List ItemField
fields =
    [ ItemField (Name Nothing) ViewMode
    , ItemField (QCount Nothing) ViewMode
    , ItemField (QUnit Nothing) ViewMode
    , ItemField (Comment Nothing) ViewMode
    , ItemField (Symbol Nothing) ViewMode
    ]


type Msg
    = StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | UpdateField ItemField (Maybe String)
