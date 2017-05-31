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

console.log('Reading JSON from ' + json_dir);

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

function getTrailText(trail) {
  return trail.map(function (trail_elt) {
    return [trail_elt['content'], trail_elt['blog']['blog_name']].join(' ');
  }).join(' ');
}

function getDialogueText(dialogue) {
  return dialogue.map(function (dialogue_elt) {
    return [dialogue_elt['name'], dialogue_elt['phrase']].join(' ');
  }).join(' ');
}

function getTextOf(post) {
  var text_fields = [ post['summary']
                      , post['question'] || ''
                      , post['answer'] || ''
                      , post['title'] || ''
                      , post['album'] || ''
                      , post['track_name'] || ''
                      , post['source'] || ''
                      , post['caption'] || ''
                      , post['text'] || ''
                      , post['link_url'] || ''
                      , post['trail'] ? getTrailText(post['trail']) : ''
                      , post['dialogue'] ? getDialogueText(post['dialogue']) : ''
                    ];
  return text_fields.join(' ');
}

fs.readdir(json_dir, function(err, files) {
  if (err) throw err;

  files.filter(x => x.endsWith(".json")).forEach(function (filename) {
    var filepath = path.join(json_dir, filename),
        doc = JSON.parse(fs.readFileSync(filepath, 'utf8'));

    doc['content'] = getTextOf(doc);
    builder.add(doc);
  });
  fs.writeFileSync(out_file, JSON.stringify(builder.build()));
});
