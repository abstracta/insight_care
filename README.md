# ai_pocs

Application showing different POCs applying GenerativeAI

![screenshot](screenshot.png)

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

### Test

To test the project you will need files in [tests/data/accounting](tests/data/accounting) containing pdf files for accounting testing and csv with expected values. Eg: my_test.pdf & my_test.csv.

To run the tests execute `devbox run test`

## Deployment

### Requirements

* Azure cli with proper environment variables setting. Check [here](https://developer.hashicorp.com/terraform/tutorials/azure-get-started/azure-build#install-the-azure-cli-tool).
* Setting `CLOUDFLARE_API_TOKEN` environment variable with a [cloudflare API token](https://developers.cloudflare.com/fundamentals/api/get-started/create-token/)

### Deploy

* copy [sample-terraform.tfvars](sample-terraform.tfvars) to `terraform.tfvars`
* set proper values in the new [terraform.tfvars](terraform.tfvars) file
* run `devbox run deploy`
* Check deployment at https://ai-pocs.abstracta.us

> Deployment sometimes fail with `waiting for Zip Deployment to complete`. Just try again in this scenarios.
