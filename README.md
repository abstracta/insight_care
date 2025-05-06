# Insight Care

AI-powered tool provides mental well-being scanning through a conversational interface, based on the WHO-5 assessment, and offers primary psychological first aids according to WHO recommendations with an empathetic tone. 

## Development

### Requirements

* [Devbox](https://www.jetpack.io/devbox)

### Setup

* run `devbox install && devbox run install`
* configure proper values in [.env](.env) generated file.
* if you want to disable authentication for local deployment set `SKIP_AUTH` to `1` in [.env](.env) file.

### Run

* run `devbox run insight_care`
* navigate to https://localhost:8000
* login, if authentication is not disabled, with `accounting` or `medpre` user and the password configured in [.env](.env) file.
* drop prescription or accounting files in file drop down and wait for the analysis results