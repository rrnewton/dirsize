

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


[2011.02.15] {Craig sent me a simple python version}

Here are some results:

    $ time du -sch /ffh/ryan/communication/

       3.1G    /ffh/ryan/communication/
       3.1G    total
       real    0m0.233s
       user    0m0.057s
       sys     0m0.138s

# OCaml:
    $ time ds /ffh/ryan/communication/ 

	 Summing /ffh/ryan/communication/     ..................
	    3,217,905,780 bytes in 7826 plain files.
		  388,586 bytes in 1193 directories.
		    1,753 bytes in 25 symlink files.
	    3,218,296,119 bytes total.
       real    0m0.290s
       user    0m0.127s
       sys     0m0.105s

# Now Haskell:
    $ time ./ds /ffh/ryan/communication/ 
      Contains 3217905780  bytes.
      real    0m0.247s
      user    0m0.125s
      sys     0m0.118s

# Now Python:
    $ time ./ds.py /ffh/ryan/communication/
       Total for /ffh/ryan/communication: 3217905780 bytes
       real    0m0.372s
       user    0m0.202s
       sys     0m0.147s

------------------------------------------------------------

For my whole ffh (sans photos):

    $ time du -sch /ffh/ryan/
       38G     /ffh/ryan/
       38G     total
       real    0m18.413s
       user    0m1.984s
       sys     0m13.025s

# OCaml:
    $ time ds /ffh/ryan/ 
      Summing /ffh/ryan/     ..................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
	39,914,917,917 bytes in 288797 plain files.
	    13,032,234 bytes in 30919 directories.
		58,786 bytes in 1748 symlink files.
	39,928,008,937 bytes total.
    real    0m16.887s
    user    0m2.541s
    sys     0m11.179s

# Haskell:
    $ time ./ds /ffh/ryan/
       Contains 39914918967  bytes.
       real    0m30.580s
       user    0m11.411s
       sys     0m13.785s


# Python: 
    $ time ./ds.py /ffh/ryan/
       Total for /ffh/ryan: 39914918805 bytes

       real    0m27.944s
       user    0m7.259s
       sys     0m14.513s

--------------------------------------------------------------------------------

Haskell GC above:

    Contains 39914918967  bytes.
       1,296,895,188 bytes allocated in the heap
	 492,248,084 bytes copied during GC
	  11,137,024 bytes maximum residency (90 sample(s))
	   7,340,600 bytes maximum slop
		  24 MB total memory in use (0 MB lost due to fragmentation)
      Generation 0:  2081 collections,     0 parallel,  7.20s,  7.57s elapsed
      Generation 1:    90 collections,     0 parallel,  0.41s,  0.45s elapsed


Hmm... as a hacking thing increasing the heap size to 500M can cut the
user time almost in half.  only 43% in GC (250 minor collections)
rather than 66% above...

    real    0m29.989s
    user    0m6.697s
    sys     0m14.192s



[2011.02.22] {Another round of timing on Wasp}
----------------------------------------------------------------------------------------------------

Counting all of /opt/:

OCaml ds:
    30,359,845,504 bytes in 685974 plain files.
       104,321,024 bytes in 18414 directories.
            24,520 bytes in 808 symlink files.
    30,464,191,048 bytes total.
    real    0m2.029s
    user    0m0.630s
    sys     0m1.400s

du:
  real    0m0.995s
  user    0m0.130s
  sys     0m0.860s

Haskell v1 (ds.hs, stack size problem):
With a 100M stack it can complete but that silly (95% time in GC):
  real    1m22.624s
  user    1m18.690s
  sys     0m3.870s
OOPS... this is using the FOLLOW SYMLINKS VERSION!!

Haskell stream_ds.hs:
  
Haskell stream_ds2.hs:
    [newton@wasp] $ time ./stream_ds2 /opt/
    30359845504
    real    0m11.787s
    user    0m10.110s
    sys     0m1.680s

Hsakell filemanip (6 line version, 27% GC):
    real    0m5.932s
    user    0m4.080s
    sys     0m1.850s

Haskell imperative.hs (5% GC):
    Total bytes: 30359845504
    real    0m3.207s
    user    0m1.650s
    sys     0m1.560s
    

Python ds.py (2.6.5):
    Total for /opt: 30359845504 bytes
    real    0m9.282s
    user    0m5.750s
    sys     0m3.530s

Python ds2.py:   
    Total for /opt: 30359845504 bytes
    real    0m9.310s
    user    0m5.720s
    sys     0m3.590s

