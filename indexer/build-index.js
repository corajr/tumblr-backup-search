var lunr = require('lunr'),
    JSONStream = require('JSONStream'),
    es = require('event-stream'),
    stdin = process.stdin,
    stdout = process.stdout;

stdin.resume();
stdin.setEncoding('utf8');

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
builder.field('body');

stdin
  .on('end', function() {
    stdout.write(JSON.stringify(builder.build()));
  })
  .pipe(JSONStream.parse('*'))
  .pipe(es.mapSync(function(doc) {
    builder.add(doc);
  }));

stdout.on('error', process.exit);
