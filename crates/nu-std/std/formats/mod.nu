# formats.nu
#
# This file contains helpers for formatting data in various ways.
#
# Usage:
#   use std/format *
#   use std/format <command name>
#
# These custom commands help `open` the files with unsupported extensions such as ndjson.
#

# Convert from NDJSON (https://github.com/ndjson/ndjson-spec) to structured data.
export def "from ndjson" []: string -> any {
    from json --objects
}

# Convert from JSONL (https://jsonlines.org/) to structured data.
export def "from jsonl" []: string -> any {
    from json --objects
}

# Convert structured data to NDJSON (https://github.com/ndjson/ndjson-spec).
export def "to ndjson" []: any -> string {
    each { to json --raw } | to text
}

# Convert structured data to JSONL (https://jsonlines.org/).
export def "to jsonl" []: any -> string {
    each { to json --raw } | to text
}

# Convert from NDNUON (newline-delimited NUON) to structured data
export def "from ndnuon" []: [string -> any] {
    lines | each { from nuon }
}

# Convert structured data to newline-delimited NUON (NDNUON)
export def "to ndnuon" []: [any -> string] {
    each { to nuon | str replace --all "\n" '\n' } | to text
}
