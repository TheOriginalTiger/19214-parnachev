data Group = ToGroup {
    gName :: String,
    gMembers :: [String]
} deriving (Show, Ord, Eq)

data Song = ToSong {
    sName :: String,
    sGroup :: Group
} deriving (Show, Ord, Eq)

data Album = ToAlbum {
    aGroup :: Group,
    aSongs :: [Song]
} deriving (Show, Ord, Eq)

data User = ToUser {
    uName :: String,
    uSongs :: [Song],
    uAlbums :: [Album],
    uGroups :: [Group]
} deriving (Show, Ord, Eq)

addUserSong :: User->Song->User
addUserSong user song = ToUser (uName user) (uSongs user ++ [song]) (uAlbums user) (uGroups user)

addUserAlbum :: User->Album->User
addUserAlbum user album = ToUser (uName user) (uSongs user ) (uAlbums user ++ [album]) (uGroups user)

addUserGroup :: User->Group->User
addUserGroup user group = ToUser (uName user) (uSongs user ) (uAlbums user) (uGroups user ++ [group])

searchSongByGroup :: User->Group->[Song]
searchSongByGroup user group = [s | s <- (uSongs user), sGroup s == group]

searchSongByName :: User->String->[Song]
searchSongByName user name = [s | s <- (uSongs user), sName s == name]

-- some staff to test it
top = ToGroup "twenty one pilots" ["Pilo1", "Pilot2"]
em = ToGroup "em" ["em"]
song1 = ToSong "Nico and the 9ers" top 
song2 = ToSong "MushvikitonBackTime" top
song3 = ToSong "Sing for the moment" em
user = ToUser "Default" [song1, song2, song3] [] [top, em]
