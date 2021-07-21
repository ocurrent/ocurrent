### The build cache

When run, OCurrent keeps its state in `./var` (creating it if it doesn't already exist).
The layout is subject to change, but for debugging purposes:

- `var/job/$date/$time-$name-$random.log` is used for build logs.
- `var/db` contains an sqlite database recording completed builds and their results.
- `var/git/$repo-$hash` is used by the Git plugin as a local Git clone for a remote repository.

For debugging, you can query the build cache (even while OCurrent is running) like this:

```
$ sqlite3 var/db/sqlite.db
sqlite> SELECT key, job_id, ok, value FROM cache WHERE op='docker-build' ORDER BY finished;
```

Here, `key` is the Git commit that was built, `log` is the relative path of the build log, 
`ok` indicates whether the build passed, and `value` is the resulting Docker image 
(or the error message, if `ok` is false).

For the full schema, see [lib_cache/db.ml](https://github.com/ocurrent/ocurrent/blob/master/lib_cache/db.ml).
