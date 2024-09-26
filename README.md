# Pharmaverse Stats
This project aims at providing latest updates on the `pharmaverse` package maintainers activities and informs the latest status of the packages.

## Structure
This repository consists of 3 folders:
1. config — contains `packages.yaml` file, listing all the `pharmaverse` package repository paths and names.
2. maintainer_diff — contains 4 files 
  - `repo_data_utils.R`: This file contains functions for fetching the repository data.
  - `data_processing_utils.R`: This file contains functions that process the repository data.
  - `data_preparation.R`: It uses the functions defined in the `data_processing_utils.R` file to fetch the `pharmaverse` package maintainers and their activities data.
  - `slackr_updates.R`: This file prepares the slack text and pushes the update using the Slack API.
3. tests - contains test script.
4. styles - contains 5 files
   - `colors.R`, `colors.scss`: colors configuration for R and Sass. Could be refactor later to have just single source of truth.
   - `custom.scss`: custom styles for the quarto document.
   - `theme.R`: theme configuration for reactable.
   - `utils.R`: utility functions.
   
`package_status_report.qmd`: This quarto document runs the `data_preparation.R` and `slackr_updates.R` scripts to generate detailed report on `pharmaverse` package status, and pushes the update on the specified Slack channel.

**Note: In case of maintainer's `CommitEvent`, we consider the `author` details of the commit, as the commit is performed by `github-actions[bot]` for some packages. [September 8, 2024]**

## Using Devcontainer
To ensure a consistent development experience across all environments, we recommend using the [devcontainer](https://code.visualstudio.com/docs/remote/containers) configuration with Visual Studio Code or DevPod for container-based development.

1. **Start the Devcontainer**:
Open the project in VS Code and select "Reopen in Container" when prompted, or use the Command Palette (`F1` or `Ctrl+Shift+P`) and choose "Dev Containers: Reopen in Container". Alternatively, use [DevPod](https://devpod.sh/) following their instructions. Once the container has been built and running, proceed to step 2.

2. **run renv restore**:
   ```r
   renv::restore()
   ```

## Setting Up Locally
For developers preferring a local setup without Devcontainer:

1. **Install correct R version**:
Ensure that the right R version is installed. This project currently uses R 4.4.1

2. **run renv restore**:
   ```r
   renv::restore()
   ```

## Tests
To run the tests:
```r
testthat::test_dir("tests")
```
