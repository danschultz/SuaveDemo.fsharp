open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Web

let html container =
    OK (View.index container)

let overview = 
    Database.getContext()
    |> Database.getGenres
    |> List.map (fun genre -> genre.Name)
    |> View.store
    |> html

let browse =
    request (fun r ->
        match r.queryParam Path.Store.browseKey with
        | Choice1Of2 genre -> 
            Database.getContext ()
            |> Database.getAlbumsForGenre genre
            |> View.browse genre
            |> html
        | Choice2Of2 msg -> BAD_REQUEST msg)

let details id =
    let context = Database.getContext ()
    match Database.getAlbumDetails id context with
    | Some albumDetails -> html (View.details albumDetails)
    | None -> never

let webPart =
    choose [
        path Path.home >=> html View.home

        path Path.Store.overview >=> overview
        path Path.Store.browse >=> browse
        pathScan Path.Store.details details

        pathRegex "(.*)\.(css|png)" >=> Files.browseHome

        html View.notFound
    ]

startWebServer defaultConfig webPart
