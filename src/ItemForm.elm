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


itemFields : List ItemField
itemFields =
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


alter :
    (ItemField -> ItemField)
    -> ItemField
    -> List ItemField
    -> List ItemField
alter mapper field fields =
    List.map
        (\existing ->
            if existing == field then
                mapper field

            else
                existing
        )
        fields


allToView : List ItemField -> List ItemField
allToView fields =
    List.map
        (\field ->
            case field of
                ItemField fieldName _ ->
                    ItemField fieldName ViewMode
        )
        fields


alterMode : FieldMode -> ItemField -> ItemField
alterMode mode field =
    case field of
        ItemField fieldName _ ->
            ItemField fieldName mode


alterContent : Maybe String -> ItemField -> ItemField
alterContent data field =
    case field of
        ItemField fieldName mode ->
            case fieldName of
                Name _ ->
                    ItemField (Name data) mode

                QUnit _ ->
                    ItemField (QUnit data) mode

                QCount _ ->
                    ItemField (QCount (Maybe.andThen String.toFloat data)) mode

                Comment _ ->
                    ItemField (Comment data) mode

                Symbol _ ->
                    ItemField (Symbol data) mode
