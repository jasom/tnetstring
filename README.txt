This is an implementation of tagged netstrings, bassed off of Zed Shaw's
Python reference implementation at  http://codepad.org/Uj42SuMo

Tagged netstrings are a strict superset of netstrings, so this code can be
used to parse ordinary netstrings as well.

Note that I have no idea how secure this implementation is, so I would
recommend you audit it before reading code from untrusted sources.  If you see
any vulnerabilities, feel free to let me know, or file a pull request.
