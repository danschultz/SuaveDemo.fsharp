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
            match state.get "cartid", state.get "username", state.get "role" with
            | Some cartId, None, None -> f (CartIdOnly cartId)
            | _, Some username, Some role ->
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
    let context = Database.getContext ()
    let result cartItems user =
        OK (View.index (View.partNav cartItems) (View.partUser user) container)
        >=> Writers.setMimeType "text/html; charset=utf-8"

    session (function
    | UserLoggedIn { Username = username } ->
        result (Database.countCartItems username context) (Some username)
    | CartIdOnly cartId ->
        result (Database.countCartItems cartId context) None
    | NoSession -> result 0 None)

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

module Cart =
    let overview =
        session (function
            | NoSession -> View.Cart.empty |> html
            | UserLoggedIn { Username = cartId } | CartIdOnly cartId ->
                let context = Database.getContext()
                Database.getCartDetails cartId context
                |> View.Cart.cart
                |> html)

    let addToCart albumId =
        let context = Database.getContext ()
        session (function
                | NoSession ->
                    let cartId = Guid.NewGuid().ToString("N")
                    Database.addToCart cartId albumId context
                    sessionStore (fun store -> store.set "cartid" cartId)
                | UserLoggedIn { Username = cartId } | CartIdOnly cartId ->
                    Database.addToCart cartId albumId context
                    succeed)
            >=> Redirection.FOUND Path.Cart.overview

    let removeFromCart albumId =
        session (function
            | NoSession -> never
            | UserLoggedIn { Username = cartId } | CartIdOnly cartId ->
                let context = Database.getContext ()
                match Database.getCart cartId albumId context with
                | Some cart ->
                    Database.removeFromCart cart albumId context
                    succeed
                | None -> never)
            >=> Redirection.FOUND Path.Cart.overview

    let checkout =
        session (function
        | NoSession -> never
        | CartIdOnly _ -> redirectionWithReturnPath Path.Account.login
        | UserLoggedIn { Username = username } ->
            choose [
                GET >=> (View.Cart.checkout |> html)
                POST >=> warbler (fun _ ->
                    let context = Database.getContext ()
                    Database.placeOrder username context
                    View.Cart.checkoutComplete |> html)
            ])

module Auth =
    let authenticateUser (user : Database.User) =
        authenticated Cookie.CookieLife.Session false
        >=> session (function
            | CartIdOnly cartId ->
                let context = Database.getContext ()
                Database.upgradeCarts (cartId, user.Username) context
                sessionStore (fun store -> store.set "cartid" "")
            | _ -> succeed)
        >=> sessionStore (fun store ->
            store.set "username" user.Username
            >=> store.set "role" user.Role)
        >=> returnPathOrHome

    let login =
        choose [
            // GET >=> (View.Account.login "" |> html)
            GET >=>
                request (fun req ->
                    match req.queryParam "returnPath" with
                    | Choice1Of2 path ->
                        let registerUrl = Path.withParam ("returnPath", path) Path.Account.register
                        View.Account.login "" registerUrl |> html
                    | _ ->
                        View.Account.login "" Path.Account.register |> html)
            POST >=> bindToForm Form.login (fun form ->
                let context = Database.getContext()
                let (Password password) = form.Password
                match Database.validateUser (form.Username, hashPassword password) context with
                | Some user -> authenticateUser user
                | _ -> (View.Account.login "Incorrect username or password" Path.Account.register |> html)
            )
        ]

    let register =
        choose [
            GET >=> (View.Account.register "" |> html)
            POST >=> bindToForm Form.register (fun form ->
                let context = Database.getContext ()
                match Database.getUser form.Username context with
                | Some _ ->
                    "Sorry this username is taken"
                    |> View.Account.register
                    |> html
                | None ->
                    let (Password password) = form.Password
                    let user = Database.newUser (form.Username, hashPassword password, form.Email) context
                    authenticateUser user)
        ]

let app =
    choose [
        path Path.Account.register >=> Auth.register
        path Path.Account.login >=> Auth.login
        path Path.Account.logout >=> reset

        path Path.home >=> html View.home
        path Path.Store.overview >=> Store.overview
        path Path.Store.browse >=> Store.browse
        pathScan Path.Store.details Store.details

        path Path.Cart.overview >=> Cart.overview
        pathScan Path.Cart.addAlbum Cart.addToCart
        pathScan Path.Cart.removeAlbum Cart.removeFromCart
        path Path.Cart.checkout >=> Cart.checkout

        path Path.Admin.manage >=> admin Admin.manage
        path Path.Admin.createAlbum >=> admin Admin.createAlbum
        pathScan Path.Admin.editAlbum (fun id -> admin (Admin.editAlbum id))
        pathScan Path.Admin.deleteAlbum (fun id -> admin (Admin.deleteAlbum id))

        pathRegex "(.*)\.(css|png)" >=> Files.browseHome

        html View.notFound
    ]

startWebServer defaultConfig app
