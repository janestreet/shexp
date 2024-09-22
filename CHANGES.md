## Release v0.17.0

* Rename `Process.pipe_pair` to `Process.pipe_both`.


# git version

- Add `Process.call_xxx` functions that are equivalent to their
  `Process.run_xxx` counterpart except that they take a single string
  list as argument (#8, David Chemouil)
- Use `;` as PATH seperator on Windows. Find executables with `.exe`
  extension on Windows. Workaround `Sys.file_exists` being true for
  directories on Windows (#16, Jonah Beckford)
