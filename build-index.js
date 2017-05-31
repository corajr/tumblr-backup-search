#!/usr/bin/env node

var lunr = require('lunr'),
    path = require('path'),
    fs = require('fs'),
    es = require('event-stream');

if (process.argv.length < 3) {
  console.log("Usage: " + __filename + " JSON_DIR");
  process.exit(-1);
}

var json_dir = process.argv[2],
    out_dir = 'static',
    out_file = path.join(out_dir, 'search_index.json');

var builder = new lunr.Builder();
builder.pipeline.add(
  lunr.trimmer,
  lunr.stopWordFilter,
  lunr.stemmer
);

builder.searchPipeline.add(
  lunr.stemmer
);

builder.ref('post_url');
builder.field('date');
builder.field('reblogged_from_name');
builder.field('content');

fs.readdir(json_dir, function(err, files) {
  if (err) throw err;
  es.readArray(files.filter(x => x.endsWith(".json")))
    .on('end', function() {
      fs.writeFileSync(out_file, JSON.stringify(builder.build()));
    })
    .pipe(es.map(function(filename, cb) {
      var filepath = path.join(json_dir, filename);
      fs.readFile(filepath, 'utf8', cb);
    }))
    .pipe(es.mapSync(JSON.parse))
    .pipe(es.mapSync(function(doc) {
      doc['content'] = JSON.stringify(doc);
      builder.add(doc);
    }));
});
