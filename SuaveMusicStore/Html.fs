module Html

open Suave.Form
open Suave.Html

let cssLink href = link [ "href", href; " rel", "stylesheet"; " type", "text/css" ]

let h2 s = tag "h2" [] [ Text s ]

let ul nodes = tag "ul" [] nodes

let li = tag "li" []

let ulAttr attr nodes = tag "ul" attr nodes

let em s = tag "em" [] [ Text s ]

let table content = tag "table" [] content

let th content = tag "th" [] content

let tr content = tag "tr" [] content

let td content = tag "td" [] content

let strong s = tag "strong" [] (text s)

let form content = tag "form" [ "method", "POST" ] content

let submitInput value = input ["type", "submit"; "value", value]

let truncate len (s : string) =
    if s.Length > len then
        s.Substring(0, len - 3) + "..."
    else s

type Field<'a> = {
    Label : string
    Html : Form<'a> -> Suave.Html.Node
}

type Fieldset<'a> = {
    Legend : string
    Fields : Field<'a> list
}

type FormLayout<'a> = {
    Fieldsets : Fieldset<'a> list
    SubmitText : string
    Form : Form<'a>
}

let renderForm (layout : FormLayout<_>) =
    form [
        for set in layout.Fieldsets ->
            tag "fieldset" [] [
                yield tag "legend" [] [ Text set.Legend ]

                for field in set.Fields do
                    yield div ["class", "editor-label"] [
                        Text field.Label
                    ]
                    yield div ["class", "editor-field"] [
                        field.Html layout.Form
                    ]
            ]

        yield submitInput layout.SubmitText
    ]

let formInput = Suave.Form.input
