open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Web

let html container =
    OK (View.index container)

module Store =

    let overview = 
        Database.getContext()
        |> Database.getGenres
        |> List.map (fun genre -> genre.Name)
        |> View.Store.store
        |> html

    let browse =
        request (fun r ->
            match r.queryParam Path.Store.browseKey with
            | Choice1Of2 genre -> 
                Database.getContext ()
                |> Database.getAlbumsForGenre genre
                |> View.Store.browse genre
                |> html
            | Choice2Of2 msg -> BAD_REQUEST msg)

    let details id =
        let context = Database.getContext ()
        match Database.getAlbumDetails id context with
        | Some albumDetails -> html (View.Store.details albumDetails)
        | None -> never

module Admin =

    let manage = warbler (fun _ -> 
        Database.getContext ()
        |> Database.getAlbumsDetails
        |> View.Admin.manage
        |> html)

    let deleteAlbum id =
        let context = Database.getContext ()
        match Database.getAlbum id context with
        | Some album -> 
            choose [
                GET >=> warbler (fun _ -> 
                    html (View.Admin.deleteAlbum album.Title))
                POST >=> warbler (fun _ ->
                    Database.deleteAlbum album context;
                    Redirection.FOUND Path.Admin.manage)
            ]
        | None -> never

let webPart =
    choose [
        path Path.home >=> html View.home
        path Path.Store.overview >=> Store.overview
        path Path.Store.browse >=> Store.browse
        pathScan Path.Store.details Store.details

        path Path.Admin.manage >=> Admin.manage
        pathScan Path.Admin.deleteAlbum Admin.deleteAlbum

        pathRegex "(.*)\.(css|png)" >=> Files.browseHome

        html View.notFound
    ]

startWebServer defaultConfig webPart
