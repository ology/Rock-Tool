package Rocktool;

use lib map { "$ENV{HOME}/sandbox/$_/lib" } qw(Data-Dataset-ChordProgressions MIDI-Bassline-Walk MIDI-Util MIDI-Drummer-Tiny Music-Duration Music-Duration-Partition);

use Moo;
use Music::Chord::Note ();
use Music::Scales qw(get_scale_notes);
use Data::Dataset::ChordProgressions ();
use MIDI::Bassline::Walk ();
use MIDI::Drummer::Tiny ();
use MIDI::Util qw(set_chan_patch midi_format);
use Music::Duration::Partition ();

has filename     => (is => 'ro', required => 1); # MIDI file name
has coctave      => (is => 'ro');
has cpatch       => (is => 'ro');
has cvolume      => (is => 'ro');
has boctave      => (is => 'ro');
has bpatch       => (is => 'ro');
has bvolume      => (is => 'ro');
has my_bpm       => (is => 'ro');
has parts        => (is => 'ro');
has phrases      => (is => 'ro');
has repeat       => (is => 'ro');
has do_drums     => (is => 'ro');
has dvolume      => (is => 'ro');
has hihat        => (is => 'ro');
has reverb       => (is => 'ro');
has do_bass      => (is => 'ro');
has my_pool      => (is => 'ro');
has my_weights   => (is => 'ro');
has my_groups    => (is => 'ro');
has bass_motifs  => (is => 'ro');
has progressions => (is => 'rw', default => sub { [] }); # bucket for named progressions
has msgs         => (is => 'rw', default => sub { [] }); # bucket for output messages
has named_parts  => (is => 'lazy'); # bucket for named parts
has drummer      => (is => 'lazy');

sub _build_named_parts {
    my ($self) = @_;
    my @parts = split /[\s,-]+/, $self->parts;
    return \@parts;
}

sub _build_drummer {
    my ($self) = @_;
    my $d = MIDI::Drummer::Tiny->new(
        file   => $self->filename,
        bars   => 4 * $self->named_parts->@* * $self->phrases,
        bpm    => $self->my_bpm,
        reverb => $self->reverb,
        volume => $self->dvolume,
    );
    return $d;
}

sub process {
    my ($self) = @_;

    $self->drummer->sync(
        sub { drums($self) },
        sub { chords($self) },
        sub { bass($self) },
    );

    $self->drummer->write;

    return $self->msgs;
}

sub drums {
    my ($self) = @_;

    return unless $self->dvolume;

    my $bars = $self->drummer->bars * $self->repeat;

    if ($self->do_drums) {
        $self->drummer->metronome44($bars, 1);
    }
    elsif ($self->hihat) {
        my $patch = $self->hihat . '_hh';
        $self->drummer->count_in({
            bars  => $bars,
            patch => $self->drummer->$patch(),
        });
    }
}

sub bass {
    my ($self) = @_;

    return unless $self->do_bass && $self->bvolume;

    set_chan_patch($self->drummer->score, 1, $self->bpatch);

    $self->drummer->score->Volume($self->bvolume);

    my $pool    = [ split /[\s,-]+/, $self->my_pool ];
    my $weights = [ split /[\s,-]+/, $self->my_weights ];
    my $groups  = [ split /[\s,-]+/, $self->my_groups ];

    my $mdp = Music::Duration::Partition->new(
        size => $self->drummer->beats,
        pool => $pool,
        $self->my_weights ? (weights => $weights) : (),
        $self->my_groups ? (groups => $groups) : (),
    );
    my @motifs = map { $mdp->motif } 1 .. $self->bass_motifs;

    my $bassline = MIDI::Bassline::Walk->new(
        octave  => $self->boctave,
        guitar  => 1,
        verbose => 0,
        scale   => sub { $_[0] =~ /^[A-G][#b]?m/ ? 'pminor' : 'pentatonic' },
    );

    for (1 .. $self->repeat * $self->phrases) {
        for my $p ($self->progressions->@*) {
            my @chords = split /-/, $p;
            my $i = 0;
            for my $chord (@chords) {
                $chord =~ s/^(.+?)\/.+/$1/;
                $chord =~ s/sus2/add9/;
                $chord =~ s/sus$/sus4/;
                $chord =~ s/6sus4/sus4/;

                my $m = $motifs[ int rand @motifs ];

                my $notes = $bassline->generate($chord, scalar(@$m));

                for my $j (0 .. $#$m) {
                    $self->drummer->note($m->[$j], $notes->[$j]);
                }

                $i++;
            }
        }
    }
}

sub chords {
    my ($self) = @_;

    set_chan_patch($self->drummer->score, 0, $self->cpatch);

    $self->drummer->score->Volume($self->cvolume);

    my $cn = Music::Chord::Note->new;

    my %data = Data::Dataset::ChordProgressions::as_hash();

    my @msgs; # Message accumulator
    my @accum; # Note accumulator
    my @progressions; # Named progression accumulator

    for my $part ($self->named_parts->@*) {
        my ($note, $section, $scale, $pool);
        # Set the pool of possible progressions given scale and section
        if ($part =~ /^([A-G][#b]?)(M|m)(v|c)$/) {
            ($note, $scale, $section) = ($1, $2, $3);
            $scale   = $scale eq 'M' ? 'major' : 'minor';
            $section = $section eq 'v' ? 'verse' : 'chorus';
            $pool    = $data{rock}{$scale}{$section};
        }

        # Set the transposition map
        my %note_map;
        @note_map{ get_scale_notes('C', $scale) } = get_scale_notes($note, $scale);

        # Get a random progression
        my $progression = $pool->[int rand @$pool];

        # Transpose the progression chords from C
        (my $named = $progression->[0]) =~ s/([A-G][#b]?)/$note_map{$1}/g;

        # Keep track of the progressions used
        push @progressions, $named;

        $named =~ s/-/ - /g;
        (my $prog = $progression->[1]) =~ s/-/ - /g;
        push @msgs, "$note $scale:<br>$named<br>$prog";

        my @chords = split /\s*-\s*/, $named;
        # Add each chord to the score
        for my $j (1 .. $self->repeat) {
            for my $chord (@chords) {
                $chord =~ s/^(.+?)\/.+/$1/;
                $chord =~ s/sus2/add9/;
                $chord =~ s/sus$/sus4/;
                $chord =~ s/6sus4/sus4/;

                my @notes = $cn->chord_with_octave($chord, $self->coctave);
                @notes = midi_format(@notes);
                push @accum, \@notes;
            }
        }
    }

    $self->msgs(\@msgs);

    # Set the progressions for use by the bassline
    $self->progressions(\@progressions);

    if ($self->cvolume) {
        # Add the chords to the score
        for my $j (1 .. $self->phrases) {
            #print join(', ', map { "[@$_]" } @accum), "\n";
            $self->drummer->note($self->drummer->whole, @$_) for @accum;
        }
    }
}

1;
