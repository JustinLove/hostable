# Hostable

A manually maintained ("curated") list of twitch channels that are good enough to host, but maybe you don't want any more in your videos feed from followed channels.

## Usage

Built using [Elm](http://elm-lang.org/)

The project uses `src/UserList.elm` to define the list of user names. This file is not part of the repo, but `src/UserList.elm.example` has a format example.

My build command:

> elm-make src/Hostable.elm --output public/hostable.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

The generated page is styled after the twitch following list. A host command is provided for copy-paste. User comments/tags can be provided in the UserLilst file.
