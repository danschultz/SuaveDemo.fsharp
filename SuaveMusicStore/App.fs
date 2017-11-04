open System

open Suave
open Suave.Authentication
open Suave.Cookie
open Suave.Filters
open Suave.Form
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.State.CookieStateStore
open Suave.Successful
open Suave.Web

type UserLoggedInSession = {
    Username : string
    Role : string
}

type Session =
    | NoSession
    | CartIdOnly of string
    | UserLoggedIn of UserLoggedInSession

let hashPassword (password : string) =
    use sha = Security.Cryptography.SHA256.Create()
    Text.Encoding.UTF8.GetBytes(password)
    |> sha.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let session f =
    statefulForSession
    >=> context (fun x ->
        match x |> HttpContext.state with
        | None -> f NoSession
        | Some state ->
            match state.get "username", state.get "role" with
            | Some username, Some role ->
                f (UserLoggedIn { Username = username; Role = role })
            | _ -> f NoSession)

let sessionStore setF = context (fun x ->
    match HttpContext.state x with
    | Some state -> setF state
    | None -> never)

let returnPathOrHome =
    request (fun x ->
        let path =
            match (x.queryParam "returnPath") with
            | Choice1Of2 path -> path
            | _ -> Path.home
        Redirection.FOUND path)

let reset =
    unsetPair SessionAuthCookie
    >=> unsetPair StateCookie
    >=> Redirection.FOUND Path.home

let redirectionWithReturnPath redirection =
    request (fun x ->
        let path = x.url.AbsolutePath
        Redirection.FOUND (redirection |> Path.withParam ("returnPath", path)))

let loggedIn f =
    authenticate
        Cookie.CookieLife.Session
        false
        (fun () -> Choice2Of2(redirectionWithReturnPath Path.Account.login))
        (fun _ -> Choice2Of2 reset)
        f

let admin f =
    loggedIn (session (function
        | UserLoggedIn { Role = "admin" } -> f
        | UserLoggedIn _ -> FORBIDDEN "Only for admins"
        | _ -> UNAUTHORIZED "Not logged in"))

let html container =
    let result user =
        OK (View.index (View.partUser user) container)
        >=> Writers.setMimeType "text/html; charset=utf-8"

    session (function
    | UserLoggedIn { Username = username } -> result (Some username)
    | NoSession -> result None)

let bindToForm form handler =
    bindReq (bindForm form) handler BAD_REQUEST

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

    let login = choose [
        GET >=> (View.Admin.login "" |> html)
        POST >=> bindToForm Form.login (fun form ->
            let context = Database.getContext()
            let (Password password) = form.Password
            match Database.validateUser (form.Username, hashPassword password) context with
            | Some user ->
                authenticated Cookie.CookieLife.Session false
                >=> session (fun _ -> succeed)
                >=> sessionStore (fun store ->
                    store.set "username" user.Username
                    >=> store.set "role" user.Role
                >=> returnPathOrHome)
            | _ -> (View.Admin.login "Incorrect username or password" |> html)
        )
    ]

    let manage = warbler (fun _ ->
        Database.getContext ()
        |> Database.getAlbumsDetails
        |> View.Admin.manage
        |> html)

    let createAlbum =
        let context = Database.getContext ()
        choose [
            GET >=> warbler (fun _ ->
                let genres =
                    Database.getGenres context
                    |> List.map (fun genre -> decimal genre.Genreid, genre.Name)
                let artists =
                    Database.getArtists context
                    |> List.map (fun artist -> decimal artist.Artistid, artist.Name)
                html (View.Admin.createAlbum genres artists))
            POST >=> bindToForm Form.album (fun form ->
                Database.createAlbum
                    (int form.ArtistId,
                     int form.GenreId,
                     form.Price,
                     form.Title) context
                Redirection.FOUND Path.Admin.manage)
        ]

    let editAlbum id =
        let context = Database.getContext ()
        match Database.getAlbum id context with
        | Some album ->
            choose [
                GET >=> warbler (fun _ ->
                    let genres =
                        Database.getGenres context
                        |> List.map (fun genre -> decimal genre.Genreid, genre.Name)
                    let artists =
                        Database.getArtists context
                        |> List.map (fun artist -> decimal artist.Artistid, artist.Name)
                    html (View.Admin.editAlbum album genres artists))
                POST >=> bindToForm Form.album (fun form ->
                    Database.updateAlbum
                        album
                        (int form.ArtistId, int form.GenreId, form.Price, form.Title)
                        context
                    Redirection.FOUND Path.Admin.manage)
            ]
        | None -> never

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

let cart = View.Cart.cart [] |> html

let app =
    choose [
        path Path.Account.login >=> Admin.login
        path Path.Account.logout >=> reset

        path Path.home >=> html View.home
        path Path.Store.overview >=> Store.overview
        path Path.Store.browse >=> Store.browse
        pathScan Path.Store.details Store.details

        path Path.Cart.overview >=> cart

        path Path.Admin.manage >=> admin Admin.manage
        path Path.Admin.createAlbum >=> admin Admin.createAlbum
        pathScan Path.Admin.editAlbum (fun id -> admin (Admin.editAlbum id))
        pathScan Path.Admin.deleteAlbum (fun id -> admin (Admin.deleteAlbum id))

        pathRegex "(.*)\.(css|png)" >=> Files.browseHome

        html View.notFound
    ]

startWebServer defaultConfig app
