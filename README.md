I'm told this is a giant pile of code which might seem unapproachable.  So as
a brief guide I would start with `NOTES.md` and then look at the top half of
`lptabz.nim`.  `TODO.md` also has a lot of notes in it.

My overarching goal/vision is to allow "the fast way" most of the time,
especially for developers that might know how to provide a good `hash`,
but to also have auto fall backs to "safer ways" with optional messages to
let developers know they might want to intervene by changing some defaults at
table construction time (or else let users/sysadmins know that some input may
be violating the assumptions of some code sensitive to inputs).  I think this
may be a thing that commercial database systems have done for decades, but
hasn't really percolated into commonly available runtime libraries.  (Depth
based growth trigger is probably the simplest example of a profile-guided
optimization for data structures.  A.Dain Samples 1993 PhD thesis has some
more.)

## Documentation
[An index of modules and their generated documentation is available on GitHub.](https://c-blake.github.io/adix/adix.html)
