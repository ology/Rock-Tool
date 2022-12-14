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
  my $coctave  = $c->param('coctave')  || 4;
  my $cpatch   = $c->param('cpatch')   // 4; # 0 - 127
  my $cvolume  = $c->param('cvolume')  // 100; # 0 - 127
  my $boctave  = $c->param('boctave')  || 1;
  my $bpatch   = $c->param('bpatch')   || 0; # 0 - 127
  my $bvolume  = $c->param('bvolume')  // 80; # 0 - 127
  my $my_bpm   = $c->param('my_bpm')   || 90;
  my $parts    = $c->param('parts')    || 'Amv DMc Emv DMc'; # <Note><Major|minor><verse|chorus> phrases
  my $phrases  = $c->param('phrases')  || 1; # number of times to repeat the parts
  my $do_drums = $c->param('do_drums') || 0;
  my $dvolume  = $c->param('dvolume')  // 60; # 0 - 127
  my $hihat    = $c->param('hihat')    // 'closed'; # '' = none!
  my $reverb   = $c->param('reverb')   // 15; # 0 - 127
  my $do_bass  = $c->param('do_bass')  || 0;
  my $pool     = $c->param('pool')     || 'dhn hn qn en'; # MIDI-Perl note durations
  my $weights  = $c->param('weights')  // '1 2 3 2'; # weights of the note duration pool
  my $groups   = $c->param('groups')   // '1 1 1 2'; # groupings of the pool notes
  my $motifs   = $c->param('motifs')   || 3; # number of bass phrases to choose from

  _purge($c); # purge defunct midi files

  my $filename = '';
  my $msgs = [];

  if ($submit) {
    $filename = '/' . time() . '.mid';

    my $rock = Rocktool->new(
      filename    => 'public' . $filename,
      coctave     => $coctave,
      cpatch      => $cpatch,
      cvolume     => $cvolume,
      boctave     => $boctave,
      bpatch      => $bpatch,
      bvolume     => $bvolume,
      my_bpm      => $my_bpm,
      parts       => $parts,
      phrases     => $phrases,
      repeat      => 1,
      do_drums    => $do_drums,
      dvolume     => $dvolume,
      hihat       => $hihat,
      reverb      => $reverb,
      do_bass     => $do_bass,
      my_pool     => $pool,
      my_weights  => $weights,
      my_groups   => $groups,
      bass_motifs => $motifs,
    );

    $msgs = $rock->process;
  }

  $c->render(
    template => 'index',
    msgs     => $msgs,
    filename => $filename,
    coctave  => $coctave,
    cpatch   => $cpatch,
    cvolume  => $cvolume,
    boctave  => $boctave,
    bpatch   => $bpatch,
    bvolume  => $bvolume,
    my_bpm   => $my_bpm,
    parts    => $parts,
    phrases  => $phrases,
    do_drums => $do_drums ? 1 : 0,
    dvolume  => $dvolume,
    hihat    => $hihat,
    reverb   => $reverb,
    do_bass  => $do_bass ? 1 : 0,
    pool     => $pool,
    weights  => $weights,
    groups   => $groups,
    motifs   => $motifs,
  );
} => 'index';

get '/help' => sub ($c) {
  $c->render(
    template => 'help',
  );
} => 'help';

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
        <input type="text" class="form-control form-control-sm" id="parts" name="parts" value="<%= $parts %>" title="Space-separated chord progression parts" aria-describedby="partsHelp">
        <small id="partsHelp" class="form-text text-muted">Form: &lt;Note>&lt;Major|minor>&lt;verse|chorus></small>
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="cpatch">Patch:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="cpatch" name="cpatch" min="0" max="127" value="<%= $cpatch %>" title="0 to 127 defining the chord patch">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="cvolume">Volume:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="cvolume" name="cvolume" min="0" max="127" value="<%= $cvolume %>" title="0 to 127 defining the chord volume">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="coctave">Octave:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="coctave" name="coctave" min="3" max="6" value="<%= $coctave %>" title="Chord octave from 3 to 6">
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
        <label for="phrases">Repeat:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="phrases" name="phrases" min="1" max="64" value="<%= $phrases %>" title="1 to 64 repeats of the given parts phrase">
      </div>
    </div>
  </div>

  <div class="row">
    <div class="col">
      <div class="form-check form-check-inline">
        <input class="form-check-input" type="checkbox" id="do_bass" name="do_bass" <%= $do_bass ? 'checked' : '' %> title="Play a randomized bassline">
        <label class="form-check-label" for="do_bass">Bass</label>
      </div>
    </div>
    <div class="col">
      <button type="button" class="btn btn-outline-dark btn-sm btn-block" data-toggle="collapse" data-target="#bassSettings">Bass Settings</button>
    </div>
  </div>

<div class="collapse" id="bassSettings">

  <p></p>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="bpatch">Patch:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="bpatch" name="bpatch" min="0" max="127" value="<%= $bpatch %>" title="0 to 127 defining the bass patch">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="bvolume">Volume:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="bvolume" name="bvolume" min="0" max="127" value="<%= $bvolume %>" title="0 to 127 defining the bass volume">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="boctave">Octave:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="boctave" name="boctave" min="1" max="4" value="<%= $boctave %>" title="Bass octave from 1 to 4">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="motifs">Motifs:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="motifs" name="motifs" min="1" max="16" value="<%= $motifs %>" title="1 to 16 bass motifs">
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="pool">Pool:</label>
      </div>
      <div class="col">
        <input type="text" class="form-control form-control-sm" id="pool" name="pool" value="<%= $pool %>" title="Allowed bass durations" aria-describedby="poolHelp">
        <small id="poolHelp" class="form-text text-muted">qn = quarter note, ten = triplet eighth, etc.</small>
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="weights">Weights:</label>
      </div>
      <div class="col">
        <input type="text" class="form-control form-control-sm" id="weights" name="weights" value="<%= $weights %>" title="Weights of bass durations" aria-describedby="weightsHelp">
        <small id="weightsHelp" class="form-text text-muted">Weights of each pool duration</small>
      </div>
    </div>
  </div>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="groups">Groups:</label>
      </div>
      <div class="col">
        <input type="text" class="form-control form-control-sm" id="groups" name="groups" value="<%= $groups %>" title="Groupings of bass durations" aria-describedby="groupsHelp">
        <small id="groupsHelp" class="form-text text-muted">Groups of pool durations (e.g. ten = 3)</small>
      </div>
    </div>
  </div>

</div>

<p></p>

  <div class="row">
    <div class="col">
      <div class="form-check form-check-inline">
        <input class="form-check-input" type="checkbox" id="do_drums" name="do_drums" <%= $do_drums ? 'checked' : '' %> title="Play a 4/4 drum pattern">
        <label class="form-check-label" for="do_drums">Drums</label>
      </div>
    </div>
    <div class="col">
      <button type="button" class="btn btn-outline-dark btn-sm btn-block" data-toggle="collapse" data-target="#drumSettings">Drum Settings</button>
    </div>
  </div>

<div class="collapse" id="drumSettings">

  <p></p>

  <div class="form-group">
    <div class="row">
      <div class="col">
        <label for="dvolume">Volume:</label>
      </div>
      <div class="col">
        <input type="number" class="form-control form-control-sm" id="dvolume" name="dvolume" min="0" max="127" value="<%= $dvolume %>" title="0 to 127 defining the drums volume">
      </div>
    </div>
  </div>

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

</div>

  <p></p>

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

@@ help.html.ep
% layout 'default';
% title 'Help!';

<p>For a list of the available patches, please see <a href="https://www.midi.org/specifications-old/item/gm-level-1-sound-set">this page</a>.</p>
<p>Many settings are self explanatory, but the <b>bass</b> deserves a bit of attention.</p>
<p>The <b>octave</b> is most naturally either <b>1</b> or <b>2</b>. Different patches sound ok at the lowest octave. Some sound better the next one up.</p>
<p><b>Motifs</b> are the number of bass phrases or "figures." These are chosen at random during the progression. The more there are, the more random the bassline is.</p>
<p>The <b>pool</b> is the required set of note durations that can happen. These are in "MIDI-Perl" format, where "hn" is a half-note, and "ten" is a triplet eighth-note, etc.</p>
<p><b>Weights</b> are the optional probabilities that the corresponding pool entries will be chosen.</p>
<p><b>Groups</b> are the optional indications for how many times to try to repeat a corresponding duration in succession.</p>

@@ layouts/default.html.ep
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href="/css/fontawesome.css" rel="stylesheet">
    <link href="/css/solid.css" rel="stylesheet">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous">
    <script src='//www.midijs.net/lib/midi.js'></script>
    <script src="https://cdn.jsdelivr.net/npm/jquery@3.5.1/dist/jquery.slim.min.js" integrity="sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj" crossorigin="anonymous"></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/js/bootstrap.min.js" integrity="sha384-+sLIOodYLS7CIrQpBjl+C7nPvqq+FbNUBDunl/OZv93DB7Ln/533i8e/mZXLi/P+" crossorigin="anonymous"></script>
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
        | <a href="/help">Help!</a>
      </div>
    </div>
  </body>
</html>
