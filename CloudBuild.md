# Cloud Build Deployment

### Deployment Triggers
- Deployments are triggered by pushing a `git` tag with the a particular regex pattern to GitHub
  - The regex pattern is the name of the environment (e.g. `dev`, `test`, `prod`) followed by `-v`, & then any characters can follow this
  - Example: `dev-v0.0.1` was the first Cloud Build deployment `git` tag
- Trigger Tag Regex Patterns:
  - `DEV`: `dev-v*`
  - `TEST`: `test-v*`

- Deployment Commands:
```
# terminal

# Tag the last commit before pushing to remote
git tag dev-v<VERSION_NUMBER> <COMMIT_SHA>

# Push commits w/ RStudio & git CLI
git push origin dev-v<VERSION_NUMBER>
```

- Cloud Build description:
  - Uses same Cloud Build configuration & Dockerfiles for all environments (currently `DEV` & `TEST`)
  - Build Files:
    - `cloudbuild.yaml`
    - `CloudBuildDockerfile`
      - Contains 1 additional line compared to `Dockerfile` to add `config.yml` from GCP
  - Build Steps:
    - (1) Add `config.yml` from GCP Secret Manager
    - (2) Build Docker image
    - (3) Push Docker image to Google Cloud Registry (`gcr.io`)
    - (4) Deploy Docker image to Cloud Run


### Delete Existing Tag

```
# terminal

# Delete local tag
git tag -d <TAG_NAME>

# Delete remote (GitHub) tag
git push origin :refs/tags/<TAG_NAME>
```
