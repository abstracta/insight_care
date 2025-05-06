# ai_pocs

Application showing different POCs applying GenerativeAI

## Development

### Requirements

* [Devbox](https://www.jetpack.io/devbox)

### Setup

* run `devbox install && devbox run install`
* configure proper values in [.env](.env) generated file.
* if you want to disable authentication for local deployment set `SKIP_AUTH` to `1` in [.env](.env) file.

### Run

* run `devbox run pocs`
* navigate to https://localhost:8000
* login, if authentication is not disabled, with `accounting` or `medpre` user and the password configured in [.env](.env) file.
* drop prescription or accounting files in file drop down and wait for the analysis results