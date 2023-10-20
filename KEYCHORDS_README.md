# KeyChords

The KeyChords module is the point of this fork.
The keyChords function is designed for creating chord progressions in a desired
key without knowing which chords should be major/minor/etc.
This function was designed with 7 note scales in mind, though it works for
any scale in the scaleTable.

The type signature is as follows:
```haskell
keyChords :: (Pattern t) => t Note -> t String -> t GenChord -> t ValueMap
```
The `GenChord` type stores the scale degree of tonic and all chord modifications rather than the individual notes in the chord. 
The conversion to notes happens last, in order to keep other operations fully independent of selected scale.

Example useage:
```haskell
d1 $ slow 4 $ keyChords "g4" "major" "1 2 4 5" # s "superfm"
```
Here, `g4` is the tonic, `major` is the mode/scale, and `1 2 4 5` are the scale degrees to build the chords on.
In other words, this will play a I-ii-IV-V chord progression in g major.
Note that the tonic and the mode/scale can be patterned too, if you would like.

The module also contains the `midiKeyChords` function, which works almost exactly the same, except it insists that all notes produced are within midi-playable range.

## Chord Modifications

You can modify chords using a hyphen `-` and the appropriate modifiers.
You can apply multiple modifiers to a chord, and they will be applied in the
same order you type them in.
Be sure to separate each modifier by a hyphen. `1-am7-i2-d` will parse, but
`1-am7i2d` will not.

### Invert

Use `-i` to invert a chord.

Inverting a chord takes the lowest note and raises it an octave.
You can invert multiple times, either by typing `i` multiple times or by
appending the number of inversions you would like.
For instance, to get the second inversion of the chord built on the 3rd scale
degree, you could type `3-ii` or `3-i2`.

### Open

Use `-o` to open a chord.

You can use multiple `o`s to spread the chord further, i.e. `5-ooo`.

### Power

Use `-p` to remove all instances of the "third" from the chord, 
creating a power chord.

In this case, the "third" is the note two scale degrees above the root of the
chord.

### Up and Down

Use `-u` or `-d` to move a chord up or down an octave, respectively.

Similar to inversions, this modification can be repeated by duplicating the
letter or by adding a number to the end.
So `1-uu` would build a chord on the 1st scale degree and raise it two octaves.
And `4-d3` would build a chord on the 4th scale degree and lower it three
octaves.

### Add

Other notes can be added to chords for flavor.
Use `-a` to add a note above the root, and use `-b` to add a note to the bass.

Notes can be added by scale degree or by interval above the root.
Use `S` to indicate that you are adding by scale degree.
So for example, `1-bS2` would build a chord on the 1st scale degree and then add
the 2nd scale degree to the bass.

Use interval notations to add by interval above the root.
For example, `5-am7` would build a chord on the 5th scale degree and then add
the note that is a minor 7th above the 5th scale degree (creating a dominant
7th chord).

## Backwards-Compatibility

Despite many additions to the parser, this is a backwards compatible change.
The new parse functions are only called when a string is coerced to a `Pattern GenChord`, which is not a type expected by any previously existing Tidal functions.
