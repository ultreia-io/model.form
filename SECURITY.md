# Security policy

Version 0.1.x receives security fixes while it is the current release series.

Report vulnerabilities privately through [GitHub advisories](https://github.com/ultreia-io/model.form/security/advisories/new).

If private reporting is unavailable, contact Tony through his GitHub profile before disclosing details publicly.

Do not include credentials, personal data, or confidential workbooks in public issues.

The package disables YAML expression evaluation and rejects tags, aliases, anchors, and merge syntax.

Resource parsing is not an isolation boundary for arbitrarily large hostile files. Callers should limit input sizes.
