# incremental-factory-utilities


To publish to the wiki you'll need a credentials file. For wiki.gg use this template:

    basic-user=<ask @severin>
    basic-pass=<ask @severin>
    wiki-user=<YourName>@<botname>
    wiki-pass=<Your Bot Password>
    url-base=https://incrementalfactory.wiki.gg
    rest-path=rest.php/v1
    api-path=api.php

For fandom:

    wiki-user=<YourName>@<botname>
    wiki-pass=<Your Bot Password>
    url-base=https://incremental-factory.fandom.com/
    rest-path=rest.php/v1
    api-path=api.php%

A bot password can be created in the wiki at `/wiki/Special:BotPasswords`

The app looks for this file in `~/incremental-factory-wiki.credentials` by default,
but this path can be overridden via a command line arg.

To run, first install SBT (use SDKMan for this on MacOS or Linux).
Then launch the console via `sbt`

There are two built-in commands:

    run analyse-save-file --intent all classpath:/savegame-reference.json

This analyses the embedded save file, to use a real one change the `classpath...`
argument to point to it.  `--intent` can be `buildings|resources|skills|all`
or just the first letter of these,and you can repeat the `--intent` argument more than once.

e.g.

    -i b -i r

The second command publishes to the wiki

    run publish-wiki --intent buildings --credentials /Users/me/incremental-factory-fandom.credentials

The intent options here are `main`, `items`, `buildings`, `parceltypes`, `skills`,  and `all`