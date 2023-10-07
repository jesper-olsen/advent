#[rustfmt::skip]
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum Mot {
    N=0, S, E, W, NE, SE, NW, SW, U, D, L, R, In, Out, Forward, Back,
    Over, Across, Upstream, Downstream, Enter, Crawl, Jump, Climb,
    Look, Cross, Road, Hill, Woods, Valley, House, Gully, Stream, 
    Depression, Entrance, Cave, Rock, Slab, Bed, Passage, Cavern, 
    Canyon, Awkward, Secret, Bedquilt, Reservoir, Giant, Oriental,
    Shell, Barren, Broken, Debris, View, Fork, Pit, Slit, Crack, Dome,
    Hole, Wall, Hall, Room, Floor, Stairs, Steps, Cobbles, Surface,
    Dark, Low, Outdoors, Y2, Xyzzy, Plugh, Plover, Office, Nowhere,
}

use Mot::*;

const N_MOTIONS: usize = Nowhere as usize + 1;

#[rustfmt::skip]
pub const MOTIONS: [Mot; N_MOTIONS] =  [
    N, S, E, W, NE, SE, NW, SW, U, D, L, R, In, Out, Forward, Back,
    Over, Across, Upstream, Downstream, Enter, Crawl, Jump, Climb,
    Look, Cross, Road, Hill, Woods, Valley, House, Gully, Stream, 
    Depression, Entrance, Cave, Rock, Slab, Bed, Passage, Cavern, 
    Canyon, Awkward, Secret, Bedquilt, Reservoir, Giant, Oriental,
    Shell, Barren, Broken, Debris, View, Fork, Pit, Slit, Crack, Dome,
    Hole, Wall, Hall, Room, Floor, Stairs, Steps, Cobbles, Surface,
    Dark, Low, Outdoors, Y2, Xyzzy, Plugh, Plover, Office, Nowhere];
