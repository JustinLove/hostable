# Hostable

A manually maintained ("curated") list of twitch channels that are good enough to host, but maybe you don't want any more in your videos feed from followed channels.

## Usage

Data is stored in browser local storage. For backup and transfer between systems, the list of users and comments/tags can be exported and imported.

## Building

Built using [Elm](http://elm-lang.org/)

A [Twitch Client-ID](https://dev.twitch.tv/docs/authentication#registration) is required to make API calls. This is defined in `src/TwitchId.elm`. This file is not part of of the repo, but `src/TwitchId.elm.example` can be copied and edited to provide your client id.

My build command:

> elm-make src/Hostable.elm --output public/hostable.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

The generated page is styled after the twitch following list. A host command is provided for copy-paste.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
Alert sound: [Short drone](https://freesound.org/people/FK_Prod/sounds/190039/) by `FK_Prod` (volume reduced)
