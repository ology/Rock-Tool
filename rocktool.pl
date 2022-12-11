#!/usr/bin/env perl
use Mojolicious::Lite -signatures;

use File::Find::Rule ();
use Time::HiRes qw(time);

use lib 'lib';
use Rocktool ();

use constant MIDI_GLOB  => '*.mid';
use constant TIME_LIMIT => 60 * 60; # 1 hour

get '/' => sub ($c) {
  my $submit   = $c->param('submit')   || 0;
  my $octave   = $c->param('octave')   || 4;
  my $cpatch   = $c->param('cpatch')   || 4;
  my $bpatch   = $c->param('bpatch')   || 0;
  my $my_bpm   = $c->param('my_bpm')   || 90;
  my $parts    = $c->param('parts')    || 'Amv-DMc-Emv-DMc'; # <Note><Major|minor><verse|chorus> phrases
  my $phrases  = $c->param('phrases')  || 1;
  my $repeat   = $c->param('repeat')   || 1;
  my $hihat    = $c->param('hihat')    // 'closed'; # '' = none!
  my $do_drums = $c->param('do_drums') || 0;
  my $do_bass  = $c->param('do_bass')  || 0;
  my $reverb   = $c->param('reverb')   // 15;

  _purge($c); # purge defunct midi files

  my $filename = '';
  my $msgs = [];

  if ($submit) {
    $filename = '/' . time() . '.mid';

    my $rock = Rocktool->new(
      filename => 'public' . $filename,
      octave   => $octave,
      cpatch   => $cpatch,
      bpatch   => $bpatch,
      my_bpm   => $my_bpm,
      parts    => $parts,
      phrases  => $phrases,
      repeat   => $repeat,
      hihat    => $hihat,
      do_drums => $do_drums,
      do_bass  => $do_bass,
      reverb   => $reverb,
    );

    $msgs = $rock->process;
  }

  $c->render(
    template => 'index',
    msgs     => $msgs,
    filename => $filename,
    octave   => $octave,
    cpatch   => $cpatch,
    bpatch   => $bpatch,
    my_bpm   => $my_bpm,
    parts    => $parts,
    phrases  => $phrases,
    repeat   => $repeat,
    hihat    => $hihat,
    do_drums => $do_drums ? 1 : 0,
    do_bass  => $do_bass ? 1 : 0,
    reverb   => $reverb,
  );
} => 'index';

app->log->level('info');

app->start;

sub _purge {
  my ($c) = @_;
  my $threshold = time() - TIME_LIMIT;
  my @files = File::Find::Rule
    ->file()
    ->name(MIDI_GLOB)
    ->ctime("<$threshold")
    ->in('public');
  for my $file (@files) {
    $c->app->log->info("Removing old file: $file");
    unlink $file;
  }
}

__DATA__

@@ index.html.ep
% layout 'default';
% title 'Rock Progression Practice Tool';

<div class="row">
  <div class="col-6">

<p></p>

<form>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="parts">Parts:</label>
      </div>
      <div class="col">
        <input type="text" class="form-control form-control-sm" id="parts" name="parts" value="<%= $parts %>" title="Chord progression parts" aria-describedby="partsHelp">
        <small id="partsHelp" class="form-text text-muted">Form: &lt;Note>&lt;Major|minor>&lt;verse|chorus></small>
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="octave">Octave:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="octave" name="octave" min="3" max="6" value="<%= $octave %>" title="Octave from 3 to 6">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="cpatch">Top patch:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="cpatch" name="cpatch" min="0" max="127" value="<%= $cpatch %>" title="0 to 127 defining the top chord patch">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="bpatch">Bass patch:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="bpatch" name="bpatch" min="0" max="127" value="<%= $bpatch %>" title="0 to 127 defining the bass patch">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="my_bpm">BPM:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="my_bpm" name="my_bpm" min="1" max="200" value="<%= $my_bpm %>" title="1 to 200 beats per minute">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="repeat">Repeat:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="repeat" name="repeat" min="1" max="64" value="<%= $repeat %>" title="1 to 64 repeats of the given parts phrase">
      </div>
    </div>
  </div>

  <div class="row">
    <div class="col">
      <div class="form-check form-check-inline">
        <input class="form-check-input" type="checkbox" id="do_bass" name="do_bass" <%= $do_bass ? 'checked' : '' %> title="Play a parallel bassline">
        <label class="form-check-label" for="do_bass">Bass</label>
      </div>
    </div>
    <div class="col">
      <div class="form-check form-check-inline">
        <input class="form-check-input" type="checkbox" id="do_drums" name="do_drums" <%= $do_drums ? 'checked' : '' %> title="Play a 4/4 drum pattern">
        <label class="form-check-label" for="do_drums">Drums</label>
      </div>
    </div>
  </div>
  <p></p>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="reverb">Reverb:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="reverb" name="reverb" min="0" max="127" value="<%= $reverb %>" title="0 to 127 drum reverb amount">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="hihat">Hihat:</label>
      </div>
      <div class="col">
        <select class="form-control form-control-sm" id="hihat" name="hihat" title="Set the hi-hat metronome patch">
          <option value="" <%= !$hihat ? 'selected' : '' %>>None</option>
% for my $i (qw(pedal closed open)) {
          <option value="<%= $i %>" <%= $i eq $hihat ? 'selected' : '' %>><%= ucfirst $i %></option>
% }
        </select>
      </div>
    </div>
  </div>

  <input type="submit" class="btn btn-sm btn-primary" name="submit" value="Generate">

</form>

  </div>
  <div class="col-6">

% if ($filename) {
    <p></p>
    MIDI: &nbsp;
    <a href="#" onClick="MIDIjs.play('<%= $filename %>');" title="Play MIDI"><i class="fa-solid fa-play"></i></a>
    &nbsp; | &nbsp;
    <a href="#" onClick="MIDIjs.stop();" title="Stop MIDI"><i class="fa-solid fa-stop"></i></a>
    &nbsp; | &nbsp;
    <a href="<%= $filename %>" title="Download MIDI"><i class="fa-solid fa-download"></i></a>
    <p></p>
    <ol>
%   for my $msg (@$msgs) {
      <li><%== $msg %></li>
%   }
    </ol>
% }

  </div>
</div>

@@ layouts/default.html.ep
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href="/css/fontawesome.css" rel="stylesheet">
    <link href="/css/solid.css" rel="stylesheet">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous">
    <script type='text/javascript' src='//www.midijs.net/lib/midi.js'></script>
    <title><%= title %></title>
    <style>
      .padpage {
        padding-top: 10px;
      }
      .small {
        font-size: small;
        color: darkgrey;
      }
    </style>
  </head>
  <body>
    <div class="container padpage">
      <h3><a href="/"><%= title %></a></h3>
      <%= content %>
      <p></p>
      <div id="footer" class="small">
        <hr>
        Built by <a href="http://gene.ology.net/">Gene</a>
        with <a href="https://www.perl.org/">Perl</a> and
        <a href="https://mojolicious.org/">Mojolicious</a>
      </div>
    </div>
  </body>
</html>
