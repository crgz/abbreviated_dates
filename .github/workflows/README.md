# Requirement for Workflows creating Pull Requests 

Pull requests created by
[peter-evans/create-pull-request](https://github.com/peter-evans/create-pull-request#action-inputs) action triggered
"on: pull_request" workflow cannot use the default GITHUB_TOKEN. The recommended workaround is to enable a repo scoped
Personal Access Token (PAT) in the repository.

Omitting this setup will trigger in the error `The process '/usr/bin/git' failed with exit code 128` as described
[here]( https://github.com/actions/checkout/issues/417#issuecomment-1271880369)

## Create the Personal Access Token

1. Go to: https://github.com/settings/tokens/new
2. Add Note: `Personal access token with workflow scope`
3. Select an Expiration option
4. Select scope: `workflow`
5. Click on the button: `Generate token` 
6. Copy the token in your clipboard
7. Optionally: Keep it safe for future reference 

## Enable the Personal Access Token in the Repository

1. Go to: https://github.com/crgz/abbreviated_dates/settings/secrets/actions
2. Click on the button: `New repository secret`
3. Paste the token from your clipboard
4. Type `PAT` in the Name field
5. Click on the button: `Add secret`
