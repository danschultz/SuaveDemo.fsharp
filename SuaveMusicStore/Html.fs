module Html

open Suave.Html

let cssLink href = link [ "href", href; " rel", "stylesheet"; " type", "text/css" ]

let h2 s = tag "h2" [] [ Text s ]

let ul nodes = tag "ul" [] nodes

let li = tag "li" []

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