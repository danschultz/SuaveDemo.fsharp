module Database

open FSharp.Data.Sql

[<Literal>]
let ConnectionString =
    "Server=localhost;"         +
    "Database=suavemusicstore;" +
    "User Id=suave;"            +
    "Password=1234;"

type Sql =
    SqlDataProvider<
        ConnectionString      = ConnectionString,
        DatabaseVendor        = Common.DatabaseProviderTypes.POSTGRESQL,
        CaseSensitivityChange = Common.CaseSensitivityChange.ORIGINAL >

type DbContext = Sql.dataContext
type Album = DbContext.``public.albumsEntity``
type Genre = DbContext.``public.genresEntity``
type AlbumDetails = DbContext.``public.albumdetailsEntity``
type Artist = DbContext.``public.artistsEntity``
type User = DbContext.``public.usersEntity``
type Cart = DbContext.``public.cartsEntity``
type CartDetails = DbContext.``public.cartdetailsEntity``

let getContext () = Sql.GetDataContext()

let validateUser (username, password) (context : DbContext) : User option =
    query {
        for user in context.Public.Users do
            where (user.Username = username && user.Password = password)
            select user
    } |> Seq.tryHead

let getUser username (context : DbContext) : User option =
    query {
        for user in context.Public.Users do
            where (user.Username = username)
            select user
    } |> Seq.tryHead

let newUser (username, password, email) (context : DbContext) =
    let user = context.Public.Users.Create(username, password, "user", email)
    context.SubmitUpdates()
    user

let getGenres (context : DbContext) : Genre list =
    context.Public.Genres |> Seq.toList

let getAlbumsForGenre genreName (context : DbContext) : Album list =
    query {
        for album in context.Public.Albums do
            join genre in context.Public.Genres on (album.Genreid = genre.Genreid)
            where (genre.Name = genreName)
            select album
    } |> Seq.toList

let getAlbumDetails id (context : DbContext) : AlbumDetails option =
    query {
        for album in context.Public.Albumdetails do
            where (album.Albumid = id)
            select album
    } |> Seq.tryHead

let getAlbumsDetails (context : DbContext) : AlbumDetails list =
    context.Public.Albumdetails
    |> Seq.toList
    |> List.sortBy (fun details -> details.Artist)

let getAlbum id (context : DbContext) : Album option =
    query {
        for album in context.Public.Albums do
            where (album.Albumid = id)
            select album
    } |> Seq.tryHead

let createAlbum (artistId, genreId, price, title) (context : DbContext) =
    context.Public.Albums.Create(artistId, genreId, price, title) |> ignore
    context.SubmitUpdates()

let updateAlbum (album : Album) (artistId, genreId, price, title) (context : DbContext) =
    album.Artistid <- artistId
    album.Genreid <- genreId
    album.Title <- title
    album.Price <- price
    context.SubmitUpdates()

let deleteAlbum (album : Album) (context : DbContext) =
    album.Delete()
    context.SubmitUpdates()

let getArtists (context : DbContext) : Artist list =
    context.Public.Artists |> Seq.toList

let getCart cartId albumId (context : DbContext) : Cart option =
    query {
        for cart in context.Public.Carts do
            where (cart.Cartid = cartId && cart.Albumid = albumId)
            select cart
    } |> Seq.tryHead

let getCarts cartId (context : DbContext) : Cart list =
    query {
        for cart in context.Public.Carts do
            where (cart.Cartid = cartId)
            select cart
    } |> Seq.toList

let getCartDetails cartId (context : DbContext) : CartDetails list =
    query {
        for cartDetails in context.Public.Cartdetails do
            where (cartDetails.Cartid = cartId)
            select cartDetails
    } |> Seq.toList

let countCartItems cartId (context : DbContext) =
    getCartDetails cartId context |> List.sumBy (fun c -> c.Count)

let addToCart cartId albumId (context : DbContext) =
    match getCart cartId albumId context with
    | Some cart -> cart.Count <- cart.Count + 1
    | None -> context.Public.Carts.Create(albumId, cartId, 1, System.DateTime.UtcNow) |> ignore
    context.SubmitUpdates()

let removeFromCart (cart : Cart) albumId (context : DbContext) =
    cart.Count <- cart.Count - 1
    if cart.Count = 0 then cart.Delete()
    context.SubmitUpdates()

let upgradeCarts (cartId, username) (context : DbContext) =
    for cart in getCarts cartId context do
        match getCart username cart.Albumid context with
        | Some existing ->
            existing.Count <- existing.Count + cart.Count
            cart.Delete()
        | None -> cart.Cartid <- username
    context.SubmitUpdates()
