# tumblr-backup-search

Search through the JSON files of posts backed up by [tumblr_backup.py][tumblr_backup].

## Usage

This requires a Tumblr backed up with tumblr_backup's `-j` option (for JSON).

First, run the indexer on the directory containing the JSON files:

```sh
cd indexer
stack build && stack exec indexer /path/to/json > ../browser/search_index.json
```

This will output an index file into the frontend folder. Then, fire up the browser:

```
cd ../indexer
npm install
npm start
```

From here, you can enter queries and get the post contents/links back to the
original.

[tumblr_backup]: https://github.com/bbolli/tumblr-utils/blob/master/tumblr_backup.md
