module Data exposing (..)

type alias Data =
    { avatar : String
    , email : String
    , last_name : String
    , first_name : String
    , id : String
    , hiddenToggleOpen : Bool
    }

initDataList : List Data
initDataList =
    [ Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "1321321" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "21329813" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "21398721" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "213432421" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "5435432435" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "5435432435" False
    , Data "www.avatar.com/user" "baunders@email" "saunders" "sally" "654987654" False
    , Data "www.avatar.com/user" "baunders@email" "saunders" "sally" "543987543" False
    , Data "www.avatar.com/user" "baunders@email" "saunders" "sally" "5435435435" False
    , Data "www.avatar.com/user" "saunders@email" "saunders" "sally" "76586765" False
    , Data "www.avatar.com/user" "saunders@email" "saunders" "sally" "675876765" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "6546876754" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "46456456" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "4848768948" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "456988561" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "418964841" False
    , Data "www.avatar.com/user" "williams@email" "williams" "henry" "4864654" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "4684887664" False
    , Data "www.avatar.com/user" "videl@email" "videl" "trent" "416487688" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "joe" "48468418616" False
    , Data "www.avatar.com/user" "parker@email" "parker" "joseph" "789789789789" False
    , Data "www.avatar.com/user" "chung@email" "chung" "xiado" "257257257" False
    , Data "www.avatar.com/user" "peters@email" "peters" "winny" "25009090972" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "james" "2572572572" False
    , Data "www.avatar.com/user" "truman@email" "truman" "alice" "12338373" False
    , Data "www.avatar.com/user" "bloggs@email" "bloggs" "james" "2528588258" False
    , Data "www.avatar.com/user" "musk@email" "musk" "zara" "6546465456" False
    ]
