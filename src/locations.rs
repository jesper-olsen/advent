use std::ops::{Index, IndexMut};

#[rustfmt::skip]
#[derive(Eq, PartialEq, PartialOrd, Hash, Copy, Clone, Debug)]
#[repr(u8)]
pub enum Loc {
    Inhand, Limbo, Road, Hill, House, Valley, Forest, Woods, Slit, Outside,
    Inside, Cobbles, Debris, Awk, Bird, Spit, Emist, Nugget, Efiss, Wfiss, 
    Wmist,
    Like1, Like2, Like3, Like4, Like5, Like6, Like7, Like8, Like9,
    Like10, Like11, Like12, Like13, Like14,
    Brink, Elong, Wlong,
    Diff0, Diff1, Diff2, Diff3, Diff4, Diff5, Diff6, Diff7, Diff8, 
    Diff9, Diff10,
    Pony, Cross, Hmk, West, South, Ns, Y2, Jumble, Windoe, Dirty, Clean, 
    Wet, Dusty, Complex, Shell, Arch, Ragged, Sac, Ante, Witt, Bedquilt,
    Cheese, Soft, E2pit, W2pit, Epit, Wpit, Narrow, Giant, Block, Immense,
    Falls, Steep, Abovep, Sjunc, Tite, Low, Crawl, Window, Oriental, Misty,
    Alcove, Proom, Droom, Slab, Abover, Mirror, Res,
    Scan1, Scan2, Scan3,
    Secret, Wide, Tight, Tall, Boulders, Scorr, Swside,
    Dead0, Dead1, Dead2, Dead3, Dead4, Dead5, Dead6, Dead7, 
    Dead8, Dead9, Dead10, Dead11, 
    Neside, Corr, Fork, Warm, View, Chamber, Lime, Fbarr, Barr, Neend, Swend,
    Crack, Neck, Lose, Cant, Climb, Check, Snaked, Thru, Duck, Sewer, Upnout,
    Didit, Ppass, Pdrop, Troll,
    Sayit0, Sayit1, Sayit2, Sayit3, Sayit4, Sayit5, Sayit6, Sayit7,
    Sayit8, Sayit9, Sayit10, Sayit11, Sayit12
}

use Loc::*;

impl Loc {
    pub fn from_u8(value: u8) -> Option<Self> {
        // Use `std::mem::transmute` for simplicity if the values align.
        if value <= Loc::Sayit12 as u8 {
            Some(unsafe { std::mem::transmute::<u8,Loc>(value) })
        } else {
            None
        }
    }
}

impl<T> Index<Loc> for [T; N_LOC] {
    type Output = T;
    fn index(&self, idx: Loc) -> &Self::Output {
        &self[idx as usize]
    }
}
impl<T> IndexMut<Loc> for [T; N_LOC] {
    fn index_mut(&mut self, idx: Loc) -> &mut Self::Output {
        &mut self[idx as usize]
    }
}

pub const MAX_NORMAL_LOC: Loc = Loc::Swend;
pub const MAX_PIRATE_LOC: Loc = Loc::Dead2;
pub const CHEST_LOC: Loc = Loc::Dead2;
pub const MESSAGE_LOC: Loc = Loc::Pony;

pub const MIN_IN_CAVE: Loc = Loc::Inside;
pub const MIN_LOWER_LOC: Loc = Loc::Emist;

pub const N_LOC: usize = Loc::Sayit12 as usize + 1;

pub fn is_pirate_territory(loc: Loc) -> bool {
    loc >= Loc::Road && loc <= Loc::Dead2
}

pub const LIGHTED: u16 = 1; // bit for a location that is not dark
pub const OIL: u16 = 2; // bit for presence of oil
const LIQUID: u16 = 4; // bit for presence of a liquid (oil or water)
const CAVE_HINT: u16 = 8; // bit for hit about trying to get in the dave
const BIRD_HINT: u16 = 16; //bit for hint about catching the bird
const SNAKE_HINT: u16 = 32; // bit for hint about dealing with the snake
const TWIST_HINT: u16 = 64; // bit for hint about being lost in a maze
const DARK_HINT: u16 = 128; // bit for hint about the dark room
const WITT_HINT: u16 = 256; // bit for hint about Witt's End

pub const fn long_desc(loc: Loc) -> &'static str {
    description(loc).long
}

pub const fn short_desc(loc: Loc) -> &'static str {
    description(loc).short
}

pub const fn condition(loc: Loc) -> u16 {
    description(loc).condition
}

pub const fn water_here(loc: Loc) -> bool {
    description(loc).condition  & (LIQUID + OIL) == LIQUID
}

pub const fn oil_here(loc: Loc) -> bool {
    description(loc).condition  &  (LIQUID + OIL) == LIQUID + OIL
}

pub const fn no_liquid_here(loc: Loc) -> bool {
    description(loc).condition & LIQUID == 0
}

struct Description {
    long: &'static str,
    short: &'static str,
    condition: u16,
}

const fn description(loc: Loc) -> Description {
    match loc {
        Road => Description {
            long: "You are standing at the end of a road before a small brick building. Around you is a forest. A small stream flows out of the building and down a gully.",
            short: "You re at end of road again.",
            condition: LIGHTED+LIQUID,
        },
        Hill => Description  {
            long: "You have walked up a hill, still in the forest. The road slopes back down the other side of the hill. There is a building in the distance.", 
            short: "You are at hill in road", 
            condition: LIGHTED},
        House => Description {
            long: "You are inside a building, a well house for a large spring.",
            short: "You are inside building.",
            condition: LIGHTED+LIQUID,
        },
        Valley => Description {
            long: "You are in a valley in the forest beside a stream tumbling along a rocky bed.",
            short: "You are in valley",
            condition: LIGHTED+LIQUID,
        },
        Forest => Description {
            long: "You are in open forest, with a deep valley to one side.", 
            short: "You are in forest.", 
            condition: LIGHTED},
        Woods => Description {
            long: "You are in open forest near both a valley and a road.", 
            short: "You are in forest.", 
            condition: LIGHTED},
        Slit => Description {
            long: "At your feet all the water of the stream splashes into a 2-inch slit in the rock. Downstream the streambed is bare rock.", 
            short: "You are slit in streambed.", 
            condition: LIGHTED+LIQUID},
        Outside=>Description {
            long: "You are in a 20-foot depression floored with bare dirt. Set into the dirt is a strong steel grate mounted in concrete. A dry streambed leads into the depression.", 
            short: "You're outside grate.", 
            condition: LIGHTED+CAVE_HINT},
        Inside=>Description { 
            long:"You are in a small chamber beneath a 3x3 steel grate to the surface. A low crawl over cobbles leads inwards to the west", 
            short: "You're below the grate.", 
            condition: LIGHTED},
        Cobbles=>Description {
            long: "You are crawling over cobbles in a low passage. There is a dim light at the east end of the passage.", 
            short: "You're in cobble crawl.", 
            condition: LIGHTED},
        Debris=>Description {
            long:"You are in a debris room filled with stuff washed in from the surface. A low wide passage with cobbles becomes plugged with mud and debris here, but an awkward canyon leads upward and west. A note on the wall says \"MAGIC WORD XYZZY\".", 
            short: "You're in debris room.", 
            condition: 0},
        Awk => Description { 
            long: "You are in an awkward sloping east/west canyon", 
            short: "", 
            condition: 0},
        Bird => Description {
            long: "You are in a splendid chamber thirty feet high. The walls are frozen rivers of orange stone. An Awkward canyon and a good passage exist from east and west sides of the chamber", 
            short: "You're in bird chamber.", 
            condition: BIRD_HINT},
        Spit => Description {
            long:"At your feet is a small pit breathing traces of white mist. An east passage ends here except for a small crack leading on.", 
            short: "You're at top of small pit.", 
            condition: 0},
        Emist => Description {
            long: "You are at one end of a vast hall stretching forward out of sight to the west. There are openings to either side. Nearby, a wide stone staircase leads downward. The hall is filled with wisps of white mist swaying to and fro almost as if alive. A cold wind blows up the stairscase. There is a passage at the top of a dome behind you.", 
            short: "You're in Hall of Mists", 
            condition: 0},
        Nugget => Description {
            long: "This is a low room with a crude note on the wall. The note says, \"You won't get it up the steps\".", 
            short: "You're in nugget of gold room.", 
            condition: 0},
        Efiss=>Description {
            long: "You are on the east bank of a fissure slicing clear across the hall. The mist is quite thick here, and the fissure is too wide to jump.", 
            short: "You're on east bank of fissure.", 
            condition: 0},
        Wfiss=>Description { 
            long: "You are on the west side of the fissure in the Hall of Mists.", 
            short: "", 
            condition:0},
        Wmist => Description {
            long: "You are at the west end of the Hall of Mists. A low wide crawl continues west and another goes north. To the south is a little passage 6 feet off the floor.", 
            short: "You're at west end of Hall of Mists.", 
            condition: 0},
        Like1 | Like2 | Like3 | Like4 | Like5 | Like6 | Like7 | Like8 | Like9 | Like10 | Like11 | Like12 | Like13 | Like14 => Description {
           long: "You are in a maze of twisty little passages, all alike.", 
           short: "",
           condition: TWIST_HINT},
        Brink => Description {
            long: "You are on the brink of a thirty-foot pit with a massive orange column down one wall. You could climb down here but you could not get back up. The maze continues at this level.", 
            short: "You're at brink of pit.", 
            condition: 0},
        Elong => Description {
            long: "You are at the east end of a very long hall apparently without side chambers. To the east a low wide crawl slanters up. To the north a round two foot hole slants down.", 
            short: "You're at east end of long hall.", 
            condition: 0},
        Wlong => Description {
            long: "You are at the west end of a very long featureless hall. The hall joins up with a narrow north/south passage.", 
            short: "You're at west end of long hall.", 
            condition: 0},
        Diff0 | Diff1 | Diff2 | Diff3 | Diff4 | Diff5 | Diff6 | Diff7 | Diff8 | Diff9 | Diff10 => Description {
            long: "You are in a maze of twisty little passages, all different.", 
            short: "", 
            condition: 0},
        Pony => Description {
            long: "Dead end.", 
            short: "", 
            condition: 0},
        Cross => Description {
            long: "You are at a crossover of a high N/S passage and a low E/W one.", 
            short: "", 
            condition: 0},
        Hmk => Description {
            long: "You are in the Hall of the Mountain King, with passages off in all directions.", 
            short: "You're in Hall of Mt King.", 
            condition: SNAKE_HINT},
        West=>Description {
            long: "You are in the west side chamber of the Hall of the Mountain King. A passage continues west and up here.", 
            short: "You're in west side chamber.", 
            condition: 0},
        South=>Description {
            long: "You are in the south side chamber.", 
            short: "",
            condition: 0},
        Ns=>Description {
            long: "You are in a low N/S passage at a hole in the floor. The hole goes down to an E/W passage.", 
            short: "You're in N/S passage.", 
            condition: 0},
        Y2 =>Description {
            long: "You are in a large room, with a passage to the south, a passage to the west, and a wall of broken rock to the east. There is a large \"Y2\" on a rock in the room's center.",
            short: "You're at \"Y2\".", 
            condition: 0},
        Jumble => Description {
            long: "You are in a jumble of rock, with cracks everywhere.", 
            short: "", 
            condition: 0},
        Windoe=>Description {
            long: "You're at a low window overlooking a huge pit, which extends up out of sight. A floor is indistinctly visible over 50 feet below. Traces of white mist cover the floor of the pit, becoming thicker to the right. Marks in the dust around the window would seem to indicate that someone has been here recently. Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room. A shadowy figure can be seen there peering back at you.", 
            short: "You're at window on pit.", 
            condition: 0},
        Dirty=>Description {
            long: "You are in a dirty broken passage. To the east is a crawl. To the west is a large passage. Above you is a hole to another passage.", 
            short: "You're in dirty passage.", 
            condition: 0},
        Clean=>Description {
            long: "You are on the brink of a small clean climbable pit. A crawl leads west.",
            short: "You're by a clean pit.",
            condition: 0},
        Wet=>Description {
            long: "You are in the bottom of a small pit with a little stream, which enters and exits through tiny slits.", 
            short: "You're in pit by stream", 
            condition: LIQUID},
        Dusty=>Description {
            long: "You are in a large room full of dusty rocks. There is a big hold in the floor. There are cracks everywhere, and a passage leading east.", 
            short: "You're in dusty rock room.", 
            condition: 0},
        Complex=>Description {
            long: "You are at a complex junction. A low hands-and-knees passage from the north joins a higher crawl from the east to make a walking passage going west. There is also a large room above. The air is damp here.", 
            short: "You're at complex junction.", 
            condition: 0},
        Shell=>Description {
            long: "You're in a large room carved out of sedimentary rock. The floor and walls are littered with bits of shells embedded in the stone. A shallow passage proceeds downward, and a somewhat steeper one leads up. A low hands-and-knees passage enters from the south.",
            short: "You're in Shell Room.",
            condition: 0},
        Arch => Description {
            long: "You are in an arched hall. A coral passage once continued up and east from here, but is now blocked by debris. The air smells of sea water.",
            short: "You're in arched hall.",
            condition: 0},
        Ragged=>Description {
            long: "You are in a long sloping corridor with ragged sharp walls.",
            short: "",
            condition: 0},
        Sac=>Description {
            long: "You are in a cul-de-sac about eight feet across.",
            short: "",
            condition: 0},
        Ante => Description{
            long: "You are in an anteroom leading to a large passage to the east. Small passages go west and up. The remnants of recent digging are evident. A sign in midair here says \"CAVE UNDER CONSTRUCTION BEYOND THIS POINT.PROCEED AT OWN RISK. [WITT CONSTRUCTION COMPANY]\"", 
            short: "You're in anteroom.", 
            condition: 0},
        Witt => Description {
            long: "You are at Witt's End. Passages lead off in \"all\" directions.", 
            short: "You're at Witt's End.", 
            condition: WITT_HINT},
        Bedquilt => Description {
            long: "You are in Bedquilt, a long east/west passage with holes everywhere. To explore at random select north, south, up or down.",
            short: "You're in Bedquilt.",
            condition: 0},
        Cheese => Description {
            long: "You are in a room whose walls resemble Swiss cheese. Obvious passages go west, east, NE and NW. Part of the room is occupied by a large bedrock block.",
            short: "You're in Swiss cheese room.", 
            condition: 0},
        Soft  => Description {
            long: "You are in the Soft Room. The walls are covered with heavy curtains, the floor with a thick pile carpet. Moss covers the ceiling.", 
            short: "You're in Soft Room.", 
            condition: 0},
        E2pit => Description {
            long: "You are at the east end of the Twopit Room. The floor here is littered with thin rock slabs, which make it easy to descend the pits. There is a path here bypassing the pits to connect passages from east and west. There are holes all over, but the only big one is on the wall directly over the west pit where you can't get to it.", 
            short: "You're at east end of Twopit Room.", 
            condition: 0},
        W2pit => Description {
            long: "You are at the west end of the Twopit Room. There is a large hole in the wall above the pit at this end of the room.", 
            short: "You're at west end of Twopit Room.", 
            condition: 0},
        Epit => Description { 
            long: "You are at the bottom of the eastern pit in the Twopit Room. There is a small pool of oil in one corner of the pit.", 
            short: "You're in east pit.", 
            condition: LIQUID+OIL},
        Wpit => Description {
            long: "You are at the bottom of the western pit in the Twopit Room. There is a large hole in the wall about 25 feet above you.",
            short: "You're in west pit.", 
            condition: 0},
        Narrow => Description {
            long: "You are in a long, narrow corridor stretching out of sight to the west. At the eastern end is a hole through which you can see a profusion of leaves.", 
            short: "You're in a narrow corridor.", 
            condition: 0},
        Giant => Description { 
            long: "You are in the Giant Room. The ceiling here is too high up for your lamp to show it. Cavernous passages lead east, north, and south. On the west wall is scrawled the inscription, \"FEE FIE FOE FOO\" [sic].", 
            short: "You're in Giant Room.", 
            condition: 0},
        Block => Description {
            long: "The passage here is blocked by a recent cave-in.", 
            short: "", 
            condition: 0},
        Immense => Description {
            long: "You are at one end of an immense north/south passage", 
            short: "", 
            condition: 0},
        Falls => Description {
            long: "You are in a magnificent cavern with a rushing stream, which cascades over a sparkling waterfall into a roaring whirlpool that disappears through a hole in the floor. Passages exist to the south and west.", 
            short: "You're in cavern with waterfall.", 
            condition: LIQUID},
        Steep => Description {
            long: "You are at the top of a steep incline above a large room. You could climb down here, but you would not be able to climb up. There is a passage leading back to the north.", 
            short: "You're at steep incline above large room.", 
            condition: 0},
        Abovep => Description {
            long: "You are in a secret N/S canyon above a sizable passage.", 
            short: "", 
            condition: 0},
        Sjunc => Description {
            long: "You are in a secret canyon at a junction of three canyos, bearing north, south and SE. The north one is as tall as the other two combined.", 
            short: "You're at junction of three secret canyons.", 
            condition: 0},
        Tite => Description {
            long: "A large stalactite extends from the roof and almost reaches the floor below. You could climb down it, and jump from it to the floor, but having done so you would be unable to reach it to climb back up.", 
            short: "You're on top of stalactite.", 
            condition: 0},
        Low => Description {
            long: "You are in a large low room. Crawls lead north, SE and SW.", 
            short: "", 
            condition: 0},
        Crawl => Description {long: "Dead end crawl.", short: "", condition: 0},
        Window => Description {
            long: "You're at a low window overlooking a huge pit, which extends up out of sight. A floor is indistinctly visible over 50 feet below. Traces of white mist cover the floor of the pit, becoming thicker to he left. Marks in the dust around the window would seem to indicate that someone has been here recently. Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room. A shadowy figure can be seen there peering back to you.", 
            short: "You're at window on pit.", 
            condition: 0},
        Oriental => Description {
            long: "This is the Oriental Room. Ancient oriental cave drawings cover the walls. A gently sloping passage leads upward to the north, another passage leads SE, and a hands-and-knees crawl leads west.", 
            short: "You're in Oriental Room.", 
            condition: 0},
        Misty => Description {
            long: "You are following a wide path around the outer edge of a large cavern. Far below, through a heavy white mist, strange splashing noises can be heard. The mist rises up through a fissure in the ceiling. The path exists to the south and west.",
            short: "You're in misty cavern.", 
            condition: 0},
        Alcove => Description {
            long: "You are in an alcove. A small NW path seems to widen after a short distance. An extremely thight tunnel leads east. It looks like a very tight squeeze. An eerie light can be seen at the other end.", 
            short: "You're in alcove", 
            condition: DARK_HINT},
        Proom => Description {
            long: "You're in a small chamber lit by an eerie green light. An extremely narrow tunnel exits to the west. A dark corridor leads NE.", 
            short: "You're in Plover Room.", 
            condition: DARK_HINT},
        Droom => Description {
            long: "You're in the Dark-Room. A corridor leading south is the only exit.", 
            short: "You're in Dark-Room", 
            condition: DARK_HINT},
        Slab => Description {
            long: "You are in a large low circular chamber whose floor is an immense slab fallen from the ceiling (Slab Room). There once where large passages to the east and west, but they are now filled with boulders. Low small passages go north and south, and the south one quickly bends west around the boulders.", 
            short: "You're in Slab Room.", 
            condition: 0},
        Abover => Description { 
            long: "You are in a secret N/S canyon above a large room.", 
            short: "", 
            condition: 0},
        Mirror => Description {
            long: "You are in a north/south canyon about 25 feet across. The floor is covered by white mist seeping in from the north. The walls extend upward for well over 100 feet. Suspended from some unseen point far above you, an enormous two-sided mirror is hanging parallel to and midway between the canyon walls. (The mirror is obviously provided for the use of the dwarves, who as you know are extremely vain.) A small window can be seen in either wall, some fifty feet up.", 
            short: "You're in mirror canyon.", 
            condition: 0},
        Res => Description{
            long: "You are at the edge of a large underground reservoir. An opaque cloud of white mist fills the room and rises rapidly upward. The lake is fed by a stream, which tumbles out of a hole in the wall about 10 feet overhead and splashes noisily into the water somewhere within the mist. The only passage goes back toward the south.", 
            short: "You're at reservoir.", 
            condition: LIQUID},
        Scan1 => Description {
            long: "You are in a secret canyon that exists to the north and east.", 
            short: "", 
            condition: 0},
        Secret => Description {
            long: "You are in a secret canyon, which here runs E/W. It crosses over a very tight canyon 15 feet below. If you go down you may not be able to get back up.", 
            short: "You're in secret E/W canyo above tight canyon.", 
            condition: 0},
        Wide => Description {
            long: "You are at a wide place in a very tight N/S canyon.", 
            short: "", 
            condition: 0},
        Tight => Description {
            long: "The canyo here becomes too tight to go further south.", 
            short: "", 
            condition: 0},
        Tall => Description {
            long: "You are in a tall E/W canyon. A low tight crawl goes 3 feet north and seems to open up.", 
            short: "You're in tall E/W canyon.", 
            condition: 0},
        Boulders => Description {
            long: "The canyon runs into a mass of boulders --- dead end.", 
            short: "", 
            condition: 0},
        Scorr => Description { 
            long: "You are in a long winding corridor sloping out of sight in both directions.", 
            short: "You're in sloping corridor.", 
            condition: 0},
        Swside => Description {
            long: "You are on one side of a large, dep chasm. A heavy white mist rising up from below obscures all view of the far side. A SW path leads away from the chasm into a winding corridor.", 
            short: "You're on SW side of chasm.", 
            condition: 0},
        Dead0 | Dead2 | Dead8 => Description {
            long: "Dead end.", 
            short: "", 
            condition: 0},
        Dead1 | Dead3 | Dead4 | Dead5 | Dead6 | Dead7 | Dead9 | Dead10| Dead11 => Description {
            long: "Dead end.", 
            short: "", 
            condition: TWIST_HINT},
        Neside => Description {
            long: "You are on the far side of the chasm. A NE path leads away from the chasm on this side.", 
            short: "You're on NE side of chasm.", 
            condition: 0},
        Corr => Description {
            long: "You're in a long east/west corridor. A faint rumbling noise can be heard in the distance.", 
            short: "You're in corridor.", 
            condition: 0},
        Fork => Description {
            long: "The path forks here. The left fork leads northeast. A dull rumbling seems to get louder in that direction. The right fork leads southeast down a gentle slope. The main corridor enters from the west.", 
            short: "You're at fork in path.", 
            condition: 0},
        Warm => Description {
            long: "The walls are quite warm here. From the north can be heard a steady roar, so loud that the entire cave seems to be trembling. Another passage leads south, and a low crawl goes east.", 
            short: "You're at junction with warm walls.", 
            condition: 0},
        View => Description {
            long: "You are on the edge of a breath-taking view. Far below you is an active volcano, from which great gouts of molten lava come surging out, cascading back down into the depths. The glowing rock fills the farthest reaches of the cavern with a blood-red glare, giving everything an eerie macabre appearance. The air is filled with flickering sparks of ash and a heavy smell of brimstone. The walls are hot to the touch, and the thundering of the volcano drowns out all other sounds. Embedded in the jagged roof far overhead are myriad twisted formations, composed of pure white alabaster, which scatter the murky light into sinister apparitions upon the walls. To one side is a deep gorge, filled with a bizarre chaos of tortured rock that seems to have been crafted by the Devil himself. An immense river of fire crashes out from the depths of the volcano, burns its way through the gorge and plummets into a bottomless pit far off to your left. To the right, an immense geyser of blistering steam erupts continuously from barren island in the center of a sulfurous lake, which bubbles ominously. The far right wall is aflame with an incandescence of its own, which lends an additional infernal splendor to the already hellish scene. A dark, foreboding passage exits to the south.", 
            short: "You're at breath-taking view", 
            condition: LIGHTED},
        Chamber => Description {
            long: "You are in a small chamber filled with large boulders. The walls are very warm, causing the air in the room to be almost stifling from the heat. The only exit is a crawl heading west, through which a low rubling noise is coming", 
            short: "You're in chamber of boulders.", 
            condition: 0},
        Lime => Description {
            long: "You are walking along a gently sloping north/south passage lined with oddly shaped limestone formations.", 
            short: "You're in limestone passage.", 
            condition: 0},
        Fbarr => Description {
            long: "You are standing at the entrance to a large, barren room. A sign posted above the entrance reads: \"CAUTION! BEAR IN ROOM!\"", 
            short: "You're in front of barren room.", 
            condition: 0},
        Barr => Description {
            long: "You are inside a barren room. The center of the room is completely empty except for some dust. Marks in the dust lead away toward the far end of the room. The only exit is the way you came in.", 
            short: "You're in barren room.", 
            condition: 0},
        Neend => Description {
            long: "You are at the northeast end of an immense room, even larger than the Giant Room. It appears to be a repository for the \"Adventure\" program. Massive torches far overhead bathe the room with smoky yellow light. Scattered about you can be seen a pile of bottles (all of them empty), a nursery of young beanstalks murmuring quietly, a bed of oysters, a bundle of black rods with rusty stars on their ends, and a collection of brass lanterns. Off to one side a great many dwarves are sleeping on the floor, snoring loudly. A sign nearby reads: \"DO NOT DISTURB THE DWARVES!\" An immense mirror is hanging against one wall, and stretches to the other end of the room, where various other sundry objects can be glimpsed dimly in the distance.", 
            short: "You're at NE end.", 
            condition: LIGHTED},
        Swend => Description {
            long: "You are at the southwest end of the repository. To one side is a pit full of fierce green snakes. On the other side is a row of small wicker cages, each of which contains a little sulking bird. In one corner is a bundle of black rods with rusty marks on their ends. A large number of velvet pillows are scattered about on the floor. A vast mirror stretches off to the northeast. At your feet is a large steel grate, next to which is a sign that reads, \"TREASURE VAULT. KEYS IN MAIN OFFICE.\"", 
            short: "You're at SW end.", 
            condition: LIGHTED},
        Crack => Description {
            long: "The crack is far too small for you to follow.", 
            short: "", 
            condition: 0},
        Neck => Description {
            long: "You are at the bottom of the pit with a broken neck.", 
            short: "", 
            condition: 0},
        Lose => Description {
            long: "You didn't make it.", 
            short: "", 
            condition: 0},
        Cant => Description {
            long: "The dome is unclimable.", 
            short: "", 
            condition: 0},
        Climb => Description { 
            long: "You clamber up the plant and scurry through the hole at the top.", 
            short: "", 
            condition: 0},
        Check => Description {
            long: "",
            short: "", 
            condition: 0},
        Snaked=> Description {
            long: "You can't get by the snake.", 
            short: "", 
            condition: 0},
        Thru | Duck => Description {
            long: "You have crawled through a very low wide passage parallel to and north of the Hall of Mists.", 
            short: "", 
            condition: 0},
        Sewer => Description {
            long: "The stream flows out through a pair of 1-foot-diameter sewer pipes. It would be advisable to use the exit.", 
            short: "", 
            condition: 0},
        Upnout => Description {
            long: "There is nothing here to climb. Use \"up\" or \"out\" to leave the pit.", 
            short: "", 
            condition: 0},
        Didit => Description {
            long: "You have climbed up the plant and out of the pit.", 
            short: "", 
            condition: 0},
        Inhand | Limbo | Scan2 | Scan3 | Ppass | Pdrop | Troll | Sayit0 | Sayit1 | Sayit2 | Sayit3 | Sayit4 | Sayit5 | Sayit6 | Sayit7 | Sayit8 | Sayit9 | Sayit10 | Sayit11 | Sayit12 => Description {
            long: "",
            short: "",
            condition: 0},
    }
}


#[cfg(test)]
mod tests {
    use crate::Loc;
    use crate::Loc::*;

    #[test]
    fn enum_from_u8_test() {
        for loc in [Inhand, Limbo, Road, Hill, House, Valley, Forest, Woods, Slit, Outside,
            Sayit8, Sayit9, Sayit10, Sayit11, Sayit12] {

            let l =  Loc::from_u8(loc as u8).expect("failed to convert u8 to Loc");
            assert_eq!(loc, l);
        }
    }
}
