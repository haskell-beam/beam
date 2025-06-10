# Release checklist

For any given package that you want to release:

[ ] Update the version in the appropriate cabal file;
[ ] Update the relevant CHANGELOG.md if necessary;
[ ] Commit and push;
[ ] Wait for the Github Actions pipeline to complete;
[ ] Download the release artifacts from the build on Github Actions;
[ ] Release to Hackage: `cabal upload --publish <release-artifact>`.
