# [Octane Server][]

Octane Server is a web front end to [Octane][].

-   [Install](#install)
-   [Run](#run)
-   [Configure](#configure)

## Install

1.  Install [Stack][].

2.  `stack setup`

3.  `stack build`

## Run

1.  `stack exec octane-server`

## Configure

Configure Octane Server with environment variables.

Name                  | Default
----                  | -------
AWS_ACCESS_KEY_ID     | -
AWS_SECRET_ACCESS_KEY | -
DATABASE              | `:memory:`
HOST                  | `127.0.0.1`
PORT                  | `8080`

[Octane Server]: https://github.com/tfausak/octane-server
[Octane]: https://github.com/tfausak/octane
