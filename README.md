# tumblr-backup-search

Search through the JSON files of posts backed up by [tumblr_backup.py][tumblr_backup].

## Requirements

* [node](https://nodejs.org/en/)
* a Tumblr backed up with tumblr_backup's `-j` option (for JSON).

## Usage

First, run the indexer on the directory containing the JSON files (try
`examples` if you haven't got one yet):

```sh
npm install
node --max-old-space-size=4096 build-index.js examples
```

This will output an index file into the `static` folder. Then, fire up the
Purescript app:

```
npm start
```

From here, you can enter queries and see links back to the original posts.

[tumblr_backup]: https://github.com/bbolli/tumblr-utils/blob/master/tumblr_backup.md
