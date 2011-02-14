

[2011.02.13] {Implemented a simple haskell version}

Just for fun, because there's the nice "directory-tree" package to
make it trivial.  Alas, it injects spurious and waseful CPU usage.
When I run it on a directory that's only 32K files, "ds" is properly
dominated by system time (from system calls), but the haskell version
is CPU bound:

    [newton@wasp ~/dropbox/git_projects/dirtree] $ time ds /ffh/craig/

      Summing /ffh/craig/    .........................................................................
	 4,939,925,624 bytes in 31889 plain files.
	    21,270,528 bytes in 5069 directories.
		     0 bytes in 0 symlink files.
	 4,961,196,152 bytes total.

    real	0m0.146s
    user	0m0.040s
    sys		0m0.100s

    [newton@wasp ~/dropbox/git_projects/dirtree] $ time ./dirsize /ffh/craig/
    Reading directory: /ffh/craig/
    Found 31889 regular files.
    Containing 4,939,925,624  bytes.

    real	0m0.601s
    user	0m0.470s
    sys	  	0m0.130s
    [newton@wasp ~/dropbox/

    [newton@wasp ~/dropbox/git_projects/dirtree] $ time du -sch /ffh/craig/
    4.7G	/ffh/craig/
    4.7G	total

    real	0m0.089s
    user	0m0.010s
    sys		0m0.080s


And of course du has very little system time.

Wow, where's the inefficiency here?

