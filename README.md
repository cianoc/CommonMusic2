# CommonMusic

Common Music is a music composition system that transforms high-level algorithmic representations of musical processes  
and structure into a variety of control protocols for sound synthesis and display. CommonMusic3 was reimplemented in Scheme
and CommonMusic2 bit rotted.

This is a fork of a form of CommonMusic2, where I pull out the pieces that I find useful. It is only supported on
SBCL and probably won't work on anything else until I rip out the MOP/CLOS stuff and modernize it.

Over time it will probably evolve in ways that make sense to may, and possibly only me. But part of that is documentation, and
some may be cleanup.

I use this for sheet music composition (with Incudine) and for MIDI and SuperCollider. At some point I will probably use it with CSound as well.

## Goals
The plan is to aggressively modernize this, keeping what is useful (the music theory and patterns stuff for the most part) and junking the rest. The :cm external interface should mostly remain unchanged, but no guarantees (any scheme style stuff will be aggressively nuked). I will also add other stuff that I find useful - and if it deviates sufficiently for CommonMusic then I will probably rename it, while keeping the heritage clear.

Internally I plan to remove as much MOP stuff, and use vectors instead of lists.

There will be zero scheduling code. I will probably not bother with the CM/Fomus integration.
