# Local Version

The `master` branch of hosted in GitHub *is* the local version of LNFL. Releases are in "Releases" and have tags. At AER, we have a single clone accessible to everyone for the purpose of having builds to which we can all point (`/nas/project/rc/rc1/lnfl_local_version`, with PGI and Intel builds), but this clone IS NOT FOR DEVELOPMENT! For developing the model, please refer to the [Development section](#Dev).

# Development <a href="Dev"></a>

"Incoming" is no more. If you have contrbutions to make, branch off of master, make your edits, push to your branch, then create a merge request as soon as your commit has been pushed. Owners and/or maintainers will then execute the merge if it is acceptable. The development steps are:

```
git clone --recursive git@github.com:AER-RC/LNFL.git
cd LNFL
git branch your_branch
git checkout your_branch
... (file modifications)
git commit -a -m 'made some changes to LNFL' # be more specific than this
git push origin your_branch
```

# Documentation

Documentation on how to run the model should be placed in the top-level `README.md`. Project specific commentary deemed important for future use should have [its own Wiki page](https://github.com/AER-RC/LNFL/wiki).
