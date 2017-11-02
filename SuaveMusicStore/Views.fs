module View

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

let index container =
    html [] [
        head [] [
            title [] "Suave Music Store"
            cssLink "/Site.css"
        ]

        body [] [
            div ["id", "header"] [
                tag "h1" [] [
                    a "/" [] [ Text "F# Suave Music Store" ]
                ]
            ]

            div ["id", "main"] container

            div ["id", "footer"] [
                Text "built with "
                a "http://fsharp.org" [] [ Text "F#" ]
                Text " and "
                a "http://suave.io" [] [ Text "Suave.IO" ]
            ]
        ]
    ]
    |> htmlToString

let home = [ Text "home" ]

let store genres = [
    h2 "Browse Genres"
    p [] [
        Text (sprintf "Select from %d genres" (List.length genres))
    ]
    ul [
        for genre in genres ->
            let url =
                Path.Store.browse
                |> Path.withParam (Path.Store.browseKey, genre)
            li [ a url [] [ Text genre ]]
    ]
]

let browse genre (albums : Database.Album list) = [ 
    h2 (sprintf "Genre: %s" genre)
    ul [
        for album in albums ->
            li [ 
                a (sprintf Path.Store.details album.Albumid) [] [ 
                    Text album.Title
                ] 
            ]
    ]
]

let details (album : Database.AlbumDetails) = [ 
    h2 album.Title
    p [] [ img [ "src", album.Albumarturl ] ]
    div ["id", "album-details"] [
        let captions = [ "Genre: ", album.Genre 
                         "Artist: ", album.Artist
                         "Price: ", album.Price.ToString("0.##") ]
        for (caption, text) in captions ->
            p [] [ 
                em caption
                Text text
            ]
    ]
]

let manage (albums : Database.AlbumDetails list) = [
    h2 "Index"
    table [
        yield tr [
            for t in ["Artist"; "Title"; "Genre"; "Price"; "Action"] -> th [ Text t ]
        ]

        for album in albums ->
            let text = [ truncate 25 album.Artist
                         truncate 25 album.Title
                         album.Genre
                         album.Price.ToString("0.##") ]
            tr [
                for t in text ->
                    td [ Text t ]

                yield td [
                    a (sprintf Path.Admin.deleteAlbum album.Albumid) [] [ Text "Delete" ]
                ]
            ]
    ]
]

let deleteAlbum albumTitle = [
    h2 "Delete Confirmation"
    p [] [
        Text "Are you sure you want to delete the album titled?"
        br []
        strong albumTitle
        Text "?"
    ]

    form [
        submitInput "Delete"
    ]

    div [] [
        a Path.Admin.manage [] [ Text "Back to list" ]
    ]
]

let notFound = [
    h2 "Page not found"
    p [] [
        Text "Could not find the request resource"
    ]
    p [] [
        a Path.home [] [ Text "Back to home" ]
    ]
]