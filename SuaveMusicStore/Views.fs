module View

open Html
open Suave.Form
open Suave.Html

let partNav cartItems =
    ulAttr ["id", "navlist"] [
        li [ a Path.home [] [ Text "Home" ] ]
        li [ a Path.Store.overview [] [ Text "Store" ] ]
        li [ a Path.Cart.overview [] [ Text (sprintf "Cart (%d)" cartItems) ] ]
        li [ a Path.Admin.manage [] [ Text "Admin" ] ]
    ]

let partUser (user : string option) =
    div ["id", "part-user"] [
        match user with
        | Some user ->
            yield Text (sprintf "Logged in as %s, " user)
            yield a Path.Account.logout [] [ Text "Logout" ]
        | None ->
            yield a Path.Account.login [] [ Text "Login" ]
            yield Text " "
            yield a Path.Account.register [] [ Text "Register" ]
    ]

let index partNav partUser container =
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
                partNav
                partUser
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

let notFound = [
    h2 "Page not found"
    p [] [
        Text "Could not find the request resource"
    ]
    p [] [
        a Path.home [] [ Text "Back to home" ]
    ]
]

module Store =

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

            yield p ["class", "button"] [
                a (sprintf Path.Cart.addAlbum album.Albumid) [] [ Text "Add to cart" ]
            ]
        ]
    ]

module Cart =
    let empty = [
        h2 "Your cart is empty"
        Text "Find some music in our "
        a Path.Store.overview [] [ Text "store" ]
        Text "!"
    ]

    let nonEmpty (carts : Database.CartDetails list) = [
        h2 "Review your cart:"
        table [
            yield tr [
                let headers = ["Album Name"; "Price"; "Quantity"; ""]
                for h in headers -> th [ Text h ]
            ]

            for cart in carts ->
                tr [
                    td [ a (sprintf Path.Store.details cart.Albumid) [] [ Text cart.Albumtitle ] ]
                    td [ Text (formatDec cart.Price) ]
                    td [ Text (cart.Count.ToString()) ]
                    td [
                        a (sprintf Path.Cart.removeAlbum cart.Albumid)
                            [ "class", "removeFromCart";
                              "data-id", cart.Albumid.ToString() ]
                            [ Text "Remove from cart" ]
                    ]
                ]

            yield tr [
                let total = carts |> List.sumBy (fun c -> c.Price * (decimal c.Count))
                for d in ["Total"; ""; ""; formatDec total] -> td [ Text d ]
            ]
        ]
    ]

    let cart = function
        | [] -> empty
        | list -> nonEmpty list

module Account =
    let login message = [
        h2 "Login"
        p [] [
            Text "Please enter your username and password. "
            a Path.Account.register [] [ Text "Register" ]
            Text " if you don't have an account yet."
        ]
        div ["id", "logon-message"] [ Text message ]
        renderForm
            { Form = Form.login
              Fieldsets =
                [ { Legend = "Account Information"
                    Fields =
                        [ { Label = "Username"
                            Html = formInput (fun f -> <@ f.Username @>) [] }
                          { Label = "Password"
                            Html = formInput (fun f -> <@ f.Password @>) [] }
                        ] } ]
              SubmitText = "Login"
            }
    ]

    let register message = [
        h2 "Register"
        p [] [ Text "Use the form below to create a new account."]
        div ["id", "register-message"] [ Text message ]
        renderForm
            { Form = Form.register
              Fieldsets =
                  [ { Legend = "Create a New Account"
                      Fields =
                          [ { Label = "User name (max 30 characters)"
                              Html = formInput (fun f -> <@ f.Username @>) [] }
                            { Label = "Email address"
                              Html = formInput (fun f -> <@ f.Email @>) [] }
                            { Label = "Password (between 6 and 20 characters)"
                              Html = formInput (fun f -> <@ f.Password @>) [] }
                            { Label = "Confirm password"
                              Html = formInput (fun f -> <@ f.ConfirmPassword @>) [] }
                            ] } ]
              SubmitText = "Register" }
    ]

module Admin =

    let manage (albums : Database.AlbumDetails list) = [
        h2 "Index"
        p [] [
            a Path.Admin.createAlbum [] [ Text "Create New" ]
        ]
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
                        a (sprintf Path.Admin.editAlbum album.Albumid) [] [ Text "Edit" ]
                        Text " | "
                        a (sprintf Path.Admin.deleteAlbum album.Albumid) [] [ Text "Delete" ]
                    ]
                ]
        ]
    ]

    let createAlbum genres artists = [
        h2 "Create"

        renderForm
            { Form = Form.album
              Fieldsets =
                [ { Legend = "Album"
                    Fields =
                      [ { Label = "Genre"
                          Html = selectInput (fun f -> <@ f.GenreId @>) genres None }
                        { Label = "Artist"
                          Html = selectInput (fun f -> <@ f.ArtistId @>) artists None }
                        { Label = "Title"
                          Html = formInput (fun f -> <@ f.Title @>) [] }
                        { Label = "Price"
                          Html = formInput (fun f -> <@ f.Price @>) [] }
                        { Label = "Album Art Url"
                          Html = formInput
                                    (fun f -> <@ f.ArtUrl @>)
                                    ["value", "/placeholder.gif"] }
                      ]
                  }
                ]
              SubmitText = "Create"
            }
    ]

    let editAlbum (album : Database.Album) genres artists = [
        h2 (sprintf "Edit %s" album.Title)

        renderForm
            { Form = Form.album
              Fieldsets =
                [ { Legend = "Album"
                    Fields =
                      [ { Label = "Genre"
                          Html = selectInput
                            (fun f -> <@ f.GenreId @>)
                            genres
                            (Some (decimal album.Genreid)) }
                        { Label = "Artist"
                          Html = selectInput
                            (fun f -> <@ f.ArtistId @>)
                            artists
                            (Some (decimal album.Artistid)) }
                        { Label = "Title"
                          Html = formInput
                            (fun f -> <@ f.Title @>)
                            ["value", album.Title] }
                        { Label = "Price"
                          Html = formInput
                            (fun f -> <@ f.Price @>)
                            ["value", formatDec album.Price] }
                        { Label = "Album Art Url"
                          Html = formInput
                                    (fun f -> <@ f.ArtUrl @>)
                                    ["value", "/placeholder.gif"] }
                      ]
                  }
                ]
              SubmitText = "Save Changes"
            }
        div [] [
            a Path.Admin.manage [] [ Text "Back to list" ]
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
