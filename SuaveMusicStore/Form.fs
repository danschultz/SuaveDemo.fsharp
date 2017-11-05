module Form

open Suave.Form

type Album = {
    ArtistId : decimal
    GenreId : decimal
    Title : string
    Price : decimal
    ArtUrl : string
}

type Login = {
    Username : string
    Password : Password
}

type Register = {
    Username : string
    Email : string
    Password : Password
    ConfirmPassword : Password
}

type Checkout = {
    FirstName : string
    LastName : string
    Address : string
    PromoCode : string option
}

let album : Form<Album> =
    Form ([
        TextProp ((fun f -> <@ f.Title @>), [ maxLength 100 ])
        TextProp ((fun f -> <@ f.ArtUrl @>), [ maxLength 100 ])
        DecimalProp ((fun f -> <@ f.Price @>), [ min 0.01M; max 100.0M; step 0.01M ])
    ], [])

let login : Form<Login> = Form([], [])

let passwordPattern = passwordRegex @"(\w){6,20}"

let passwordsMatch =
    (fun f -> f.Password = f.ConfirmPassword), "Passwords much match"

let register : Form<Register> =
    Form ([ TextProp ((fun f -> <@ f.Username @>), [ maxLength 30 ] )
            PasswordProp ((fun f -> <@ f.Password @>), [ passwordPattern ] )
            PasswordProp ((fun f -> <@ f.ConfirmPassword @>), [ passwordPattern ] )
            ],[ passwordsMatch ])

let checkout : Form<Checkout> = Form([], [])
