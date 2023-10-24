# Amogus

An ergonomic DSL (Domain Specific Language) written in Haskell that utilizes 
[Allen's Interval Algebra](https://github.com/Archaversine/allen) to find 
logical inconsistencies in people's alibis. 

## Documentation

There are three main statements in the DSL: `list`, `set`, and `query`.

### Local & Global Events

There are two types of events that the DSL focuses on. First there is a local 
event. This is a type of event that has a person attached to it. For example, 
Bob completing the wires task would be notated as `bob.wires`.

A global event is an event that doesn't have a person attached to it. For instance, 
pushing the button to call a meeting or when a body was reported. These are 
signified with a `#` symbol, such as `#button` or `#report`.

### List Statement

To define a sequence of actions that a person performed you can use the `list`
command to list them all out in order. The syntax is as follows:

```
list <personName> <action1> [action2] [action3] ... 
```

For example, in order to notate that player Bob first did wires, then lights, 
and then comms, you would simply write the following:

```
list bob wires lights comms
```

Note that this and all commands are terminated by a newline, so all events must 
be defined on the same line.

### Set Statement

To manually specify relations between two events, you can use the `set` statements.
The syntax is as follows:

```
set <event1> <relation symbol> <event2>
```

For example, if bob's wire task occurred before Alice's light's task, you would 
use the following:

```
set bob.wires -> alice.lights
```

The symbols that can be used are:

| Symbol | Description |
| --- | --- |
| `-> `| Precedes |
| `->|`| Meets |
| `=_ `| Overlaps |
| `-=|`| Finished By |
| `-=-`| Contains |
| `=>_`| Starts |
| `=`  | Equals |
| `=>-`| Started By |
| `_=_`| During |
| `_=|`| Finishes |
| `_=-`| Overlappped By |
| `|->`| Met By |
| `<-` | Preceded By |
| `/=` | Precedes, Meets, Met By, Preceded By |
| `~`  | Overlaps, Finished By, Contains, Starts, Equals, Started By, During, Finishes, Overlapped By |
| `~>` | Precedes, Meets, Overlaps |
| `<~` | Preceded By, Met By, Overlapped By |

### Querying

To print the current relations between two events, you can use the `query` statement.
The syntax is as follows:

```
query <event1>, <event2>
```

So for example, to see when bob's wire task was relative to the button being
pushed, the following code can be used:

```
query bob.wires, #button
```

### Name Mangling

Event names use name mangling in order to distinguish between different events, 
even if they have the same name. For example, in a game one player may do one 
task on two separate occasions, such as if they were interrupted by a button push.

To get around this, you can divide your statements with `---` so that all event  
names above the line are different than all event names at the bottom even if you 
give them the exact same name. Consider the following example: 

```
list bob wires medical trash #button

---

list bob trash admin #button
```

Both `trash` and `#button` are treated differently even though they are given 
the same name. To reference an event from a different section, you can add `+` 
or `-` to the end of the event name to reference the below and above events 
respectively. Consider:

```
list bob wires medical trash+ #button

--- 

list bob #button- trash admin #button
```

Now the `trash+` in the first section actually refers to the `trash` event in 
the second section, and the `#button-` in the second section actually refers 
to the `#button` in the first section.

### Inconsistency Detection 

If someone is not a good liar, they may make mistakes in how they tell their alibi.
For example, they may lay out a series of events that is not even logically 
possible to occur. The most basic example of this is stating that you were in the same 
room as another person when they claim you were not. In code, this would be 
expressed as the following: 

```
list bob admin wires electrical medbay #button
list alice wires admin medbay #button

set bob.wires = alice.wires
set bob.medbay /= alice.medbay

set bob.medbay = alice.medbay
```

Notice how there are two statements that contradict each other: one of them 
states that bob and alice completed the medbay task at the same time, and the 
other claims that they didn't. The interpreter can identify this inconsistency 
and prints all the related information to it:

```
[*] --== INCONSISTENCY ==--

bob.admin0
bob.wires0
bob.electrical0
bob.medbay0
#button0
alice.wires0
alice.admin0
alice.medbay0

[*] --== INCONSISTENCY ==--
amogus: On Command: set bob.medbay0 [Equals] alice.medbay0
```

This prints out all events that are related to the consistency, and the 
statement that caused the inconsisteny. This is obviously a very simple example, 
but for larger scenarios it becomes increasingly more difficult to keep track 
of this, leading to the creation of this DSL.
