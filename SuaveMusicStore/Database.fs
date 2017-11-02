module Database

open FSharp.Data.Sql

[<Literal>]
let ConnectionString = 
    "Server=localhost;"           + 
    "Database=suavemusicstore;" + 
    "User Id=suave;"           + 
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

let getContext () = Sql.GetDataContext()

let getGenres (context : DbContext) : Genre list =
    context.Public.Genres |> Seq.toList

let getAlbumsForGenre genreName (context : DbContext) : Album list =
    query {
        for album in context.Public.Albums do
            join genre in context.Public.Genres on (album.Genreid = genre.Genreid)
            where (genre.Name = genreName)
            select album
    }
    |> Seq.toList

let getAlbumDetails id (context : DbContext) : AlbumDetails option =
    query {
        for album in context.Public.Albumdetails do
            where (album.Albumid = id)
            select album
    }
    |> Seq.tryHead

let getAlbumsDetails (context : DbContext) : AlbumDetails list =
    context.Public.Albumdetails
    |> Seq.toList
    |> List.sortBy (fun details -> details.Artist)

let getAlbum id (context : DbContext) : Album option =
    query {
        for album in context.Public.Albums do
            where (album.Albumid = id)
            select album
    }
    |> Seq.tryHead

let deleteAlbum (album : Album) (context : DbContext) =
    album.Delete()
    context.SubmitUpdates()