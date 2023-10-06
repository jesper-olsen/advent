use rand::random;
use std::cmp::Ordering;
use std::collections::hash_map::{Entry, HashMap};
use std::io::{self, Write};
use std::ops::{Index, IndexMut};
use std::process;

const OK: &str = "OK.";
const PITCH_DARK_MSG: &str =
    "It is now pitch dark. If you proceed you will most likely fall into a pit.";

const SAYIT: [&str; 13] = [
    "You don't fit through a two-inch slit!",
    "You can't go through a locked steel grate!",
    "I respectfully suggest you go across the bridge instead of jumping.",
    "There is no way across the fissure.",
    "You can't fit this five-foot clam through that little passage!",
    "You can't fit this five-foot oyster through that little passage!",
    "You have crawled around in some little holes and wound up back in the main passage",
    "It is too far up for you to reach.",
    "The door is extremely rusty and refuses to open.",
    "The dragon looks rather nasty. You'd best not try to get by.",
    "The troll refuses to let you cross.",
    "There is no longer any way across the chasm.",
    "Don't be ridiculous!",
];

static INCANTATION: [&str; 5] = ["fee", "fie", "foe", "foo", "fum"];
const MAX_DEATHS: usize = 3;
static DEATH_WISHES: [&str; 2*MAX_DEATHS] = [
"Oh␣dear, you seem to have gotten yourself killed. I might be␣able to help you out, but I've never really done this before. Do you want me to try to reincarnate you?", 
"All right. But don't blame me if something goes wr......  −−− POOF!! −−− You are engulfed in a cloud of orange smoke. Coughing and gasping, you emerge from the smoke and find....",
"You clumsy oaf, you've done it again! I don't know how long I can keep this up. Do you want me to try reincarnating you again?",
"Okay, now where did I put my resurrection␣kit?....  >POOF!< Everything␣disappears in a dense cloud of orange smoke.",
"Now you've really done it! I'm out of orange smoke! You don't expect me to do a decent reincarnation without any orange smoke, do you?",
"Okay, if you're so smart, do it yourself! I'm leaving!"];

fn get_input() -> String {
    let mut s = String::new();
    io::stdin().read_line(&mut s).ok();
    String::from(s.trim())
}

fn yes(q: &str, y: &str, n: &str) -> bool {
    loop {
        print!("{q}\n** ");
        let _ = io::stdout().flush();
        if let Some(c) = get_input().chars().nth(0) {
            match c {
                'Y' | 'y' => {
                    if y != "" {
                        println!("{y}");
                        return true;
                    }
                }
                'N' | 'n' => {
                    if n != "" {
                        println!("{n}");
                        return false;
                    }
                }
                _ => println!(" Please answer Yes or No."),
            }
        }
    }
}

fn wcap(s: String) -> String {
    s.chars().take(5).collect()
}

fn listen() -> Vec<String> {
    loop {
        print!("* ");
        let _ = io::stdout().flush();
        let words: Vec<String> = get_input()
            .to_lowercase()
            .split_whitespace()
            .map(|s| s.to_string())
            .map(|s| s.chars().take(5).collect())
            .collect();

        match words.len() {
            0 => println!(" Tell me to do something."),
            1 | 2 => return words,
            _ => println!(" Please stick to 1− and 2−word commands."),
        }
    }
}

fn lookup_word(word: &str) -> Word {
    match word {
        "north" | "n" => Word::Motion(Mot::N),
        "south" | "s" => Word::Motion(Mot::S),
        "east" | "e" => Word::Motion(Mot::E),
        "west" | "w" => Word::Motion(Mot::W),
        "ne" => Word::Motion(Mot::NE),
        "se" => Word::Motion(Mot::SE),
        "sw" => Word::Motion(Mot::SW),
        "upwar" | "up" | "u" | "above" | "ascen" => Word::Motion(Mot::U),
        "downw" | "down" | "d" | "desce" => Word::Motion(Mot::D),
        "left" => Word::Motion(Mot::L),
        "right" => Word::Motion(Mot::R),
        "inwar" | "insid" | "in" => Word::Motion(Mot::In),
        "out" | "outsi" | "exit" | "leave" => Word::Motion(Mot::Out),
        "forwa" | "conti" | "onwar" => Word::Motion(Mot::Forward),
        "back" | "retur" | "retre" => Word::Motion(Mot::Back),
        "over" => Word::Motion(Mot::Over),
        "acros" => Word::Motion(Mot::Across),
        "upstr" => Word::Motion(Mot::Upstream),
        "downs" => Word::Motion(Mot::Downstream),
        "enter" => Word::Motion(Mot::Enter),
        "crawl" => Word::Motion(Mot::Crawl),
        "jump" => Word::Motion(Mot::Jump),
        "climb" => Word::Motion(Mot::Climb),
        "look" | "exami" | "touch" | "descr" => Word::Motion(Mot::Look),
        "cross" => Word::Motion(Mot::Cross),
        "road" => Word::Motion(Mot::Road),
        "hill" => Word::Motion(Mot::Hill),
        "forest" => Word::Motion(Mot::Woods),
        "valle" => Word::Motion(Mot::Valley),
        "build" | "house" => Word::Motion(Mot::House),
        "gully" => Word::Motion(Mot::Gully),
        "strea" => Word::Motion(Mot::Stream),
        "depre" => Word::Motion(Mot::Depression),
        "entra" => Word::Motion(Mot::Entrance),
        "cave" => Word::Motion(Mot::Cave),
        "rock" => Word::Motion(Mot::Rock),
        "slab" | "slabr" => Word::Motion(Mot::Slab),
        "bed" => Word::Motion(Mot::Bed),
        "passa" | "tunne" => Word::Motion(Mot::Passage),
        "caver" => Word::Motion(Mot::Cavern),
        "canyo" => Word::Motion(Mot::Canyon),
        "awkwa" => Word::Motion(Mot::Awkward),
        "secre" => Word::Motion(Mot::Secret),
        "bedqu" => Word::Motion(Mot::Bedquilt),
        "reser" => Word::Motion(Mot::Reservoir),
        "giant" => Word::Motion(Mot::Giant),
        "orien" => Word::Motion(Mot::Oriental),
        "shell" => Word::Motion(Mot::Shell),
        "barre" => Word::Motion(Mot::Barren),
        "broke" => Word::Motion(Mot::Broken),
        "debri" => Word::Motion(Mot::Debris),
        "view" => Word::Motion(Mot::View),
        "fork" => Word::Motion(Mot::Fork),
        "pit" => Word::Motion(Mot::Pit),
        "slit" => Word::Motion(Mot::Slit),
        "crack" => Word::Motion(Mot::Crack),
        "dome" => Word::Motion(Mot::Dome),
        "hole" => Word::Motion(Mot::Hole),
        "wall" => Word::Motion(Mot::Wall),
        "hall" => Word::Motion(Mot::Hall),
        "room" => Word::Motion(Mot::Room),
        "floor" => Word::Motion(Mot::Floor),
        "stair" => Word::Motion(Mot::Stairs),
        "steps" => Word::Motion(Mot::Steps),
        "cobbl" => Word::Motion(Mot::Cobbles),
        "surfa" => Word::Motion(Mot::Surface),
        "dark" => Word::Motion(Mot::Dark),
        "low" => Word::Motion(Mot::Low),
        "outdo" => Word::Motion(Mot::Outdoors),
        "y2" => Word::Motion(Mot::Y2),
        "xyzzy" => Word::Motion(Mot::Xyzzy),
        "plugh" => Word::Motion(Mot::Plugh),
        "plove" => Word::Motion(Mot::Plover),
        "main" | "offic" => Word::Motion(Mot::Office),
        "null" | "nowhe" => Word::Motion(Mot::Nowhere),
        "key" | "keys" => Word::Object(Obj::Keys),
        "lamp" | "lante" | "headl" => Word::Object(Obj::Lamp),
        "grate" => Word::Object(Obj::Grate),
        "cage" => Word::Object(Obj::Cage),
        "rod" => Word::Object(Obj::Rod),
        "bird" => Word::Object(Obj::Bird),
        "door" => Word::Object(Obj::Door),
        "pillo" => Word::Object(Obj::Pillow),
        "snake" => Word::Object(Obj::Snake),
        "fissu" => Word::Object(Obj::Crystal),
        "table" => Word::Object(Obj::Tablet),
        "clam" => Word::Object(Obj::Clam),
        "oyste" => Word::Object(Obj::Oyster),
        "magaz" | "issue" | "spelu" | "\"spel" => Word::Object(Obj::Mag),
        "dwarf" => Word::Object(Obj::Dwarf),
        "knife" => Word::Object(Obj::Knife),
        "food" => Word::Object(Obj::Food),
        "bottl" => Word::Object(Obj::Bottle),
        "water" | "h2o" => Word::Object(Obj::Water),
        "oil" => Word::Object(Obj::Oil),
        "mirro" => Word::Object(Obj::Mirror),
        "plant" => Word::Object(Obj::Plant),
        "stala" => Word::Object(Obj::Stalactite),
        "shado" | "figur" => Word::Object(Obj::Shadow),
        "axe" => Word::Object(Obj::Axe),
        "drawi" => Word::Object(Obj::Art),
        "pirat" => Word::Object(Obj::Pirate),
        "drago" => Word::Object(Obj::Dragon),
        "chasm" => Word::Object(Obj::Bridge),
        "troll" => Word::Object(Obj::Troll),
        "bear" => Word::Object(Obj::Bear),
        "messa" => Word::Object(Obj::Message),
        "volca" | "geyse" => Word::Object(Obj::Geyser),
        "vendi" | "machi" => Word::Object(Obj::Pony),
        "batte" => Word::Object(Obj::Batteries),
        "moss" | "carpe" => Word::Object(Obj::Moss),
        "gold" | "nugge" => Word::Object(Obj::Gold),
        "diamo" => Word::Object(Obj::Diamonds),
        "silve" | "bars" => Word::Object(Obj::Silver),
        "jewel" => Word::Object(Obj::Jewels),
        "coins" => Word::Object(Obj::Coins),
        "chest" | "box" | "treas" => Word::Object(Obj::Chest),
        "eggs" | "egg" | "nest" => Word::Object(Obj::Eggs),
        "tride" => Word::Object(Obj::Trident),
        "ming" | "vase" | "shard" | "potte" => Word::Object(Obj::Vase),
        "emera" => Word::Object(Obj::Emerald),
        "plati" | "pyram" => Word::Object(Obj::Pyramid),
        "pearl" => Word::Object(Obj::Pearl),
        "persi" | "rug" => Word::Object(Obj::Rug),
        "spice" => Word::Object(Obj::Spices),
        "chain" => Word::Object(Obj::Chain),
        "take" | "carry" | "keep" | "catch" | "captu" | "steal" | "get" | "tote" => {
            Word::Action(Act::Take)
        }
        "drop" | "relea" | "free" | "disca" | "dump" => Word::Action(Act::Drop),
        "open" | "unloc" => Word::Action(Act::Open),
        "close" | "lock" => Word::Action(Act::Close),
        "light" | "on" => Word::Action(Act::On),
        "extin" | "off" => Word::Action(Act::Off),
        "wave" | "shake" | "swing" => Word::Action(Act::Wave),
        "calm" | "placa" | "tame" => Word::Action(Act::Calm),
        "walk" | "run" | "trave" | "go" | "proce" | "explo" | "goto" | "follo" | "turn" => {
            Word::Action(Act::Go)
        }
        "nothi" => Word::Action(Act::Relax),
        "pour" => Word::Action(Act::Pour),
        "eat" | "devou" => Word::Action(Act::Eat),
        "drink" => Word::Action(Act::Drink),
        "rub" => Word::Action(Act::Rub),
        "throw" | "toss" => Word::Action(Act::Toss),
        "wake" | "distu" => Word::Action(Act::Wake),
        "feed" => Word::Action(Act::Feed),
        "fill" => Word::Action(Act::Fill),
        "break" | "smash" | "shatt" => Word::Action(Act::Break),
        "blast" | "deton" | "ignit" | "blowu" => Word::Action(Act::Blast),
        "attac" | "kill" | "fight" | "hit" | "strik" | "slay" => Word::Action(Act::Kill),
        "say" | "chant" | "sing" | "utter" | "mumbl" => Word::Action(Act::Say),
        "read" | "perus" => Word::Action(Act::Read),
        "fee" | "fie" | "foe" | "foo" | "fum" => Word::Action(Act::Feefie),
        "brief" => Word::Action(Act::Brief),
        "find" | "where" => Word::Action(Act::Find),
        "inven" => Word::Action(Act::Inventory),
        "score" => Word::Action(Act::Score),
        "quit" => Word::Action(Act::Quit),
        "abra" | "abrac" | "opens" | "sesam" | "shaza" | "hocus" | "pocus" => 
            Word::Message("Good try but that is an old worn-out magic word."),
        "help" | "?" => Word::Message("I know of places, actions and things. Most of my vocabulary describes places and is used to move you there. To move, try words like forest, building, downstream, enter, east, west, north, south, up or down. I know about a few special objects like a black rod hidden in the cave. These objects can be manipulated using some of the action words that I know. Usually you will need to give both the object and action words (in either order), but sometimes I can infer the object from the verb alone. Some objects also imply verbs; in particular, \"inventory\" implies \"take inventory\", which causes me to give you a list of what you are carrying. The objects have side effects; for instance, the rod scares the bird. Usually people having trouble moving just need to try a few more words. Usually people trying unsuccessfully to manipulate an object are attempting something beyond their (or my!) capabilities and should try a completely different tack. To speed the game you can sometimes move long distances with a single word. For example, \"building\" usually gets you to the building from anywhere above ground except when lost in the forest. Also, note that cave passages turn a lot, and that leaving a room to the north does not guarantee entering the next from the south. Good luck!"),
        "tree" | "trees" => Word::Message("The trees of the forest are large hardwood oak and maple, with an occasional grove of pine or spruce. There is quite a bit of undergrowth, largely birch and ash saplings plus nondescript bushes of various sorts. This time of year visibility is quite restricted by all the leaves, but travel is quite easy if you detour around the spruce and berry bushes."),
        "dig" | "excav" => Word::Message("Digging without a shovel is quite impractical. Even with a shovel progress is unlikely"),
        "lost" => Word::Message("I am as confused as you are"),
        "mist" => Word::Message("Mist is a white vapor, usually water, seen from time to time in caverns. It can be found anywhere but is frequently a sign of a deep pit leading down to water."),
        "fuck" => Word::Message("Watch it!"),
        "stop" => Word::Message("I do not know the word \"stop\". Use \"quit\" if you want to give up."),
        "info" | "infor" => Word::Message("If you want to end your adventure early say \"quit\". To get full credit for a treasure, you must have left it safely in the building, though you get partial credit just for locating it. You lose points for getting killed, or for quitting, though the former costs you more. There are also points based on how much (if any) of the cave you've managed to explore; in particular, there is a large bonus just for getting in (to distinguish the beginners from the rest of the pack), and there are other ways to determine whether you've been through some of the more harrowing sections. If you think you have found all the treasures, just keep exploring for a while. If nothing interestinghappens, you have not found them all yet. If something interesting DOES happen, it means you're getting a bonus and have an oppertunity to garner many more points in the master's section. I may occasionally offer hints if you seem to be having trouble. If I do, I will warn you in advance how much it will affect your score to accept the hints. Finally, to save paper, you may specify \"brief\", which tells me never to repeat the full description of a place unless you explicitly ask me to."),
        "swim" => Word::Message("I do not know how."),
        _ => Word::None,
    }
}

#[derive(PartialEq, Debug)]
enum Word {
    None,
    Motion(Mot),
    Object(Obj),
    Action(Act),
    Message(&'static str),
}

#[rustfmt::skip]
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Mot {
    N, S, E, W, NE, SE, NW, SW, U, D, L, R, In, Out, Forward, Back,
    Over, Across, Upstream, Downstream, Enter, Crawl, Jump, Climb,
    Look, Cross, Road, Hill, Woods, Valley, House, Gully, Stream, 
    Depression, Entrance, Cave, Rock, Slab, Bed, Passage, Cavern, 
    Canyon, Awkward, Secret, Bedquilt, Reservoir, Giant, Oriental,
    Shell, Barren, Broken, Debris, View, Fork, Pit, Slit, Crack, Dome,
    Hole, Wall, Hall, Room, Floor, Stairs, Steps, Cobbles, Surface,
    Dark, Low, Outdoors, Y2, Xyzzy, Plugh, Plover, Office, Nowhere,
}

#[rustfmt::skip]
#[derive(Eq, PartialEq, PartialOrd, Hash, Copy, Clone, Debug)]
enum Obj {
    Nothing, Keys, Lamp, Grate, Cage, Age, Rod, Rod2, Treads, 
    Bird, Door, Pillow, Snake, Crystal, 
    Tablet, Clam, Oyster, Mag, Dwarf, Knife, Food, Bottle, Water, Oil,
    Mirror, Plant, Plant2, Stalactite, Shadow, 
    Axe, Art, Pirate, Dragon, Bridge, Troll, Troll2, 
    Bear, Message, Geyser, Pony, Batteries, Moss, Gold, Diamonds, Silver,
    Jewels, Coins, Chest, Eggs, Trident, Vase, Emerald, Pyramid, Pearl,
    Rug, Spices, Chain,
}
const N_OBJECTS: usize = Obj::Chain as usize + 1;

#[rustfmt::skip]
const TREASURES: [Obj;15] = [ Obj::Gold, Obj::Diamonds, Obj::Silver,
        Obj::Jewels, Obj::Coins, Obj::Chest, Obj::Eggs, Obj::Trident,
        Obj::Vase, Obj::Emerald, Obj::Pyramid, Obj::Pearl, Obj::Rug,
        Obj::Spices, Obj::Chain];

impl<T> Index<Obj> for [T; N_OBJECTS] {
    type Output = T;
    fn index(&self, idx: Obj) -> &Self::Output {
        &self[idx as usize]
    }
}
impl<T> IndexMut<Obj> for [T; N_OBJECTS] {
    fn index_mut(&mut self, idx: Obj) -> &mut Self::Output {
        &mut self[idx as usize]
    }
}

#[rustfmt::skip]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Act {
    Abstain, Take, Drop, Open, Close, On, Off, Wave, Calm, Go, Relax,
    Pour, Eat, Drink, Rub, Toss, Wake, Feed, Fill, Break, Blast, Kill,
    Say, Read, Feefie, Brief, Find, Inventory, Score, Quit,
}

fn print_default_msg(action: Act) {
    if default_msg(action) != "" {
        println!("{}", default_msg(action));
    }
}

const fn default_msg(action: Act) -> &'static str {
    match action {
        Act::Abstain => "",
        Act::Blast => "Blasting requires dynamite.",
        Act::Brief => "On what?",
        Act::Break => "It is beyond your power to do that",
        Act::Calm => "I'm game. Would you care to explain how?",
        Act::Close => "I don't know how to lock or unlock such a thing.",
        Act::Drink => {
            "You have taken a drink from the stream. The water \
                  tastes strongly of minerals, but is not unpleasant. \
                  It is extremely cold."
        }
        Act::Eat | Act::Kill => "Don't be ridiculous!",
        Act::Feed => "There is nothing here to eat.",
        Act::Feefie => "I don’t know how.",
        Act::Fill => "You can't fill that.",
        Act::Find | Act::Inventory => {
            "I can only tell you what you see as you move \
                         about and manipulate things. I cannot tell you \
                         where remote things are."
        }
        Act::Go => "Where?",
        Act::On | Act::Off => "You have no source of light.",
        Act::Open => "I don't know how to lock or unlock such a thing.",
        Act::Pour | Act::Drop => "You aren't carrying it!",
        Act::Read => "I'm afraid I don't understand.",
        Act::Relax => "OK.",
        Act::Rub => {
            "Rubbing the electric lamp is not particularly rewarding. \
                Anyway, nothing exciting happens."
        }
        Act::Say => "",
        Act::Score | Act::Quit => "Eh?",
        Act::Take => "You are already carrying it!",
        Act::Toss => "Peculiar. Nothing unexpected happens.",
        Act::Wake => "Don't be ridiculous!",
        Act::Wave => "Nothing happens.",
    }
}

#[rustfmt::skip]
#[derive(Eq, PartialEq, PartialOrd, Hash, Copy, Clone, Debug)]
enum Loc {
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
    Sayit8, Sayit9, Sayit10, Sayit11, Sayit12,
    Nowhere
}

#[rustfmt::skip]
static LOCATIONS: [Loc; N_LOC] = [
    Loc::Inhand, Loc::Limbo, Loc::Road, Loc::Hill, Loc::House, 
    Loc::Valley, Loc::Forest, Loc::Woods, Loc::Slit, Loc::Outside,
    Loc::Inside, Loc::Cobbles, Loc::Debris, Loc::Awk, Loc::Bird,
    Loc::Spit, Loc::Emist, Loc::Nugget, Loc::Efiss, Loc::Wfiss, Loc::Wmist,
    Loc::Like1, Loc::Like2, Loc::Like3, Loc::Like4, Loc::Like5, 
    Loc::Like6, Loc::Like7, Loc::Like8, Loc::Like9, Loc::Like10, 
    Loc::Like11, Loc::Like12, Loc::Like13, Loc::Like14, 
    Loc::Brink, Loc::Elong, Loc::Wlong,
    Loc::Diff0, Loc::Diff1, Loc::Diff2, Loc::Diff3, Loc::Diff4,
    Loc::Diff5, Loc::Diff6, Loc::Diff7, Loc::Diff8, Loc::Diff9,
    Loc::Diff10,
    Loc::Pony, Loc::Cross, Loc::Hmk, Loc::West, Loc::South, Loc::Ns, 
    Loc::Y2, Loc::Jumble, Loc::Windoe, Loc::Dirty, Loc::Clean, 
    Loc::Wet, Loc::Dusty, Loc::Complex, Loc::Shell, Loc::Arch, 
    Loc::Ragged, Loc::Sac, Loc::Ante, Loc::Witt, Loc::Bedquilt,
    Loc::Cheese, Loc::Soft, Loc::E2pit, Loc::W2pit, Loc::Epit, Loc::Wpit, 
    Loc::Narrow, Loc::Giant, Loc::Block, Loc::Immense,
    Loc::Falls, Loc::Steep, Loc::Abovep, Loc::Sjunc, Loc::Tite, Loc::Low, 
    Loc::Crawl, Loc::Window, Loc::Oriental, Loc::Misty,
    Loc::Alcove, Loc::Proom, Loc::Droom, Loc::Slab, Loc::Abover, 
    Loc::Mirror, Loc::Res, Loc::Scan1, Loc::Scan2, Loc::Scan3,
    Loc::Secret, Loc::Wide, Loc::Tight, Loc::Tall, Loc::Boulders, 
    Loc::Scorr, Loc::Swside,
    Loc::Dead0, Loc::Dead1, Loc::Dead2, Loc::Dead3,
    Loc::Dead4, Loc::Dead5, Loc::Dead6, Loc::Dead7,
    Loc::Dead8, Loc::Dead9, Loc::Dead10, Loc::Dead11,
    Loc::Neside, Loc::Corr, Loc::Fork, Loc::Warm, Loc::View, 
    Loc::Chamber, Loc::Lime, Loc::Fbarr, Loc::Barr, Loc::Neend, 
    Loc::Swend
];

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

const MAX_PIRATE_LOC: Loc = Loc::Dead2;
const CHEST_LOC: Loc = Loc::Dead2;
const MESSAGE_LOC: Loc = Loc::Pony;

const MIN_IN_CAVE: Loc = Loc::Inside;
const MIN_LOWER_LOC: Loc = Loc::Emist;

const N_LOC: usize = Loc::Swend as usize + 1;

fn is_pirate_territory(loc: Loc) -> bool {
    loc >= Loc::Road && loc <= Loc::Dead2
}

const LIGHTED: u16 = 1; // bit for a location that is not dark
const OIL: u16 = 2; // bit for presence of oil
const LIQUID: u16 = 4; // bit for presence of a liquid (oil or water)
const CAVE_HINT: u16 = 8; // bit for hit about trying to get in the dave
const BIRD_HINT: u16 = 16; //bit for hint about catching the bird
const SNAKE_HINT: u16 = 32; // bit for hint about dealing with the snake
const TWIST_HINT: u16 = 64; // bit for hint about being lost in a maze
const DARK_HINT: u16 = 128; // bit for hint about the dark room
const WITT_HINT: u16 = 256; // bit for hint about Witt's End
const N_HINTS: usize = 8;

const HINT_THRESH: [u16; N_HINTS] = [0, 0, 4, 5, 8, 75, 25, 20]; // turns
const HINT_COST: [i32; N_HINTS] = [5, 10, 2, 2, 2, 4, 5, 3];

static HINT: [(&str,&str); N_HINTS] = [
   ("Welcome to Adventure!! Would you like instructions?", 
   "Somewhere nearby is␣Colossal Cave, where others have found fortunes in \
   treasure and gold, though it is rumored that some who enter are never \
   seen again. Magic is said to work in the cave. I will be your eyes \
   and hands. Direct me with commands of one or two words. I should \
   warn you that I look at only the first five letters of each word, so \
   you'll have to enter \"NORTHEAST\" as \"NE\" to distinguish it from \
   \"NORTH\". Should you get stuck, type \"HELP\" for some general hints. \
   For information on how to end your adventure, etc., type \"INFO\". \
   \n                        −  −  −\nThe first adventure program was\
   developed by Willie Crowther. Most of the features of the current \
   program were added by Don Woods."),

   ("Hmmm, this looks like a clue, which means it’ll cost you 10 points to read it. Should I go ahead and read it anyway?", 
    "It says, \"There is something strange about this place, such that one \
   of the words I've always known now has a new effect.\""),

   ("Are you trying to get into the cave?", 
    "The grate is very solid and has a hardened steel lock. You cannot \
   enter without a key, and there are no keys in sight. I would recommend \
   looking elsewhere for the keys."),

  ("Are you trying to catch the␣bird?", 
    "Something seems to be frightening the bird just now and you cannot \
   catch it no matter what you try. Perhaps you might try later."),

  ("Are you trying to deal somehow with the␣snake?", 
    "You can't kill the snake, or drive it away, or avoid it, or anything \
   like that. There is a way to get by, but you don't have the necessary \
   resources right now."),

  ("Do you need help getting out of the maze?", 
    "You can make the passages look less alike by dropping things."),

  ("Are you trying to explore beyond the Plover Room?", 
    "There is a way to explore that region without having to worry about \
   falling into a pit. None of the objects available is immediately \
   useful for discovering the secret."),

  ("Do you need help getting out of here?", "Don't go west."),
];

const fn long_desc(loc: Loc) -> &'static str {
    lookup_location(loc).0
}

const fn short_desc(loc: Loc) -> &'static str {
    lookup_location(loc).1
}

const fn condition(loc: Loc) -> u16 {
    lookup_location(loc).2
}

const fn water_here(loc: Loc) -> bool {
    condition(loc) & (LIQUID + OIL) == LIQUID
}

const fn oil_here(loc: Loc) -> bool {
    condition(loc) & (LIQUID + OIL) == LIQUID + OIL
}

const fn no_liquid_here(loc: Loc) -> bool {
    condition(loc) & LIQUID == 0
}

fn pct(n: u8) -> bool {
    random::<u8>() % 100 < n
}

fn ran(n: usize) -> usize {
    random::<usize>() % n
}

const fn lookup_location(loc: Loc) -> (&'static str, &'static str, u16) {
    match loc {
        Loc::Road => ("You are standing at the end of a road before a small brick building. Around you is a forest. A small stream flows out of the building and down a gully.", "You’re at end of road again.", LIGHTED + LIQUID ),
        Loc::Hill => ("You have walked up a hill, still in the forest. The road slopes back down the other side of the hill. There is a building in the distance.", "You are at hill in road", LIGHTED),
        Loc::House => ("You are inside a building, a well house for a large spring.", "You are inside building.", LIGHTED+LIQUID),
        Loc::Valley => ("You are in a valley in the forest beside a stream tumbling along a rocky bed.", "You are in valley", LIGHTED+LIQUID),
        Loc::Forest => ("You are in open forest, with a deep valley to one side.", "You are in forest.", LIGHTED),
        Loc::Woods => ("You are in open forest near both a valley and a road.", "You are in forest.", LIGHTED),
        Loc::Slit => ("At your feet all the water of the stream splashes into a 2-inch slit in the rock. Downstream the streambed is bare rock.", "You are slit in streambed.", LIGHTED+LIQUID),
        Loc::Outside=>("You are in a 20-foot depression floored with bare dirt. Set into the dirt is a strong steel grate mounted in concrete. A dry streambed leads into the depression.", "You're outside grate.", LIGHTED+CAVE_HINT),
        Loc::Inside=>("You are in a small chamber beneath a 3x3 steel grate to the surface. A low crawl over cobbles leads inwards to the west", "You're below the grate.", LIGHTED),
        Loc::Cobbles=>("You are crawling over cobbles in a low passage. There is a dim light at the east end of the passage.", "You're in cobble crawl.", LIGHTED),
        Loc::Debris=>("You are in a debris room filled with stuff washed in from the surface. A low wide passage with cobbles becomes plugged with mud and debris here, but an awkward canyon leads upward and west. A note on the wall says \"MAGIC WORD XYZZY\".", "You're in debris room.", 0),
        Loc::Awk => ("You are in an awkward sloping east/west canyon", "", 0),
        Loc::Bird => ("You are in a splendid chamber thirty feet high. The walls are frozen rivers of orange stone. An Awkward canyon and a good passage exist from east and west sides of the chamber", "You're in bird chamber.", BIRD_HINT),
        Loc::Spit => ("At your feet is a small pit breathing traces of white mist. An east passage ends here except for a small crack leading on.", "You're at top of small pit.", 0),
        Loc::Emist => ("You are at one end of a vast hall stretching forward out of sight to the west. There are openings to either side. Nearby, a wide stone staircase leads downward. The hall is filled with wisps of white mist swaying to and fro almost as if alive. A cold wind blows up the stairscase. There is a passage at the top of a dome behind you.", "You're in Hall of Mists", 0),
        Loc::Nugget => ("This is a low room with a crude note on the wall. The note says, \"You won't get it up the steps\".", "You're in nugget of gold room.", 0),
        Loc::Efiss=>( "You are on the east bank of a fissure slicing clear across the hall. The mist is quite thick here, and the fissure is too wide to jump.", "You're on east bank of fissure.", 0),
        Loc::Wfiss=>( "You are on the west side of the fissure in the Hall of Mists.", "", 0),
        Loc::Wmist => ("You are at the west end of the Hall of Mists. A low wide crawl continues west and another goes north. To the south is a little passage 6 feet off the floor.", "You're at west end of Hall of Mists.", 0),
        Loc::Like1 | Loc::Like2 | Loc::Like3 | Loc::Like4 | Loc::Like5 |
        Loc::Like6 | Loc::Like7 | Loc::Like8 | Loc::Like9 | Loc::Like10 |
        Loc::Like11 | Loc::Like12 | Loc::Like13 | Loc::Like14
       => ("","You are in a maze of twisty little passages, all alike.",TWIST_HINT),
        Loc::Brink => ("You are on the brink of a thirty-foot pit with a massive orange column down one wall. You could climb down here but you could not get back up. The maze continues at this level.", "You're at brink of pit.", 0),
        Loc::Elong => ("You are at the east end of a very long hall apparently without side chambers. To the east a low wide crawl slanters up. To the north a round two foot hole slants down.", "You're at east end of long hall.", 0),
        Loc::Wlong => ("You are at the west end of a very long featureless hall. The hall joins up with a narrow north/south passage.", "You're at west end of long hall.", 0),
        Loc::Diff0 | Loc::Diff1 | Loc::Diff2 | Loc::Diff3 | Loc::Diff4 |
        Loc::Diff5 | Loc::Diff6 | Loc::Diff7 | Loc::Diff8 | Loc::Diff9 |
        Loc::Diff10 =>
           ("You are in a maze of twisty little passages, all different.", "", 0),
        Loc::Pony => ("Dead end.", "", 0),
        Loc::Cross => ("You are at a crossover of a high N/S passage and a low E/W one.", "", 0),
        Loc::Hmk => ("You are in the Hall of the Mountain King, with passages off in all directions.", "You're in Hall of Mt King.", SNAKE_HINT),
        Loc::West=>("You are in the west side chamber of the Hall of the Mountain King. A passage continues west and up here.", "You're in west side chamber.", 0),
        Loc::South=>("You are in the south side chamber.","",0),
        Loc::Ns=>("You are in a low N/S passage at a hole in the floor. The hole goes down to an E/W passage.", "You're in N/S passage.", 0),
        Loc::Y2 =>("You are in a large room, with a passage to the south, a passage to the west, and a wall of broken rock to the east. There is a large \"Y2\" on a rock in the room's center.","You're at \"Y2\".", 0),
        Loc::Jumble => ("You are in a jumble of rock, with cracks everywhere.", "", 0),
        Loc::Windoe=>("You're at a low window overlooking a huge pit, which extends up out of sight. A floor is indistinctly visible over 50 feet below. Traces of white mist cover the floor of the pit, becoming thicker to the right. Marks in the dust around the window would seem to indicate that someone has been here recently. Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room. A shadowy figure can be seen there peering back at you.", "You're at window on pit.", 0),
        Loc::Dirty=>("You are in a dirty broken passage. To the east is a crawl. To the west is a large passage. Above you is a hole to another passage.", "You're in dirty passage.", 0),
        Loc::Clean=>("You are on the brink of a small clean climbable pit. A crawl leads west.","You're by a clean pit.",0),
        Loc::Wet=>("You are in the bottom of a small pit with a little stream, which enters and exits through tiny slits.", "You're in pit by stream", LIQUID),
        Loc::Dusty=>("You are in a large room full of dusty rocks. There is a big hold in the floor. There are cracks everywhere, and a passage leading east.", "You're in dusty rock room.", 0),
        Loc::Complex=>("You are at a complex junction. A low hands-and-knees passage from the north joins a higher crawl from the east to make a walking passage going west. There is also a large room above. The air is damp here.", "You're at complex junction.", 0),
        Loc::Shell=>("You're in a large room carved out of sedimentary rock. The floor and walls are littered with bits of shells embedded in the stone. A shallow passage proceeds downward, and a somewhat steeper one leads up. A low hands-and-knees passage enters from the south.","You're in Shell Room.",0),
        Loc::Arch => ("You are in an arched hall. A coral passage once continued up and east from here, but is now blocked by debris. The air smells of sea water.","You're in arched hall.",0),
        Loc::Ragged=>("You are in a long sloping corridor with ragged sharp walls.","",0),
        Loc::Sac=>("You are in a cul-de-sac about eight feet across.","",0),
        Loc::Ante => ("You are in an anteroom leading to a large passage to the east. Small passages go west and up. The remnants of recent digging are evident. A sign in midair here says \"CAVE UNDER CONSTRUCTION BEYOND THIS POINT.PROCEED AT OWN RISK. [WITT CONSTRUCTION COMPANY]\"", "You're in anteroom.", 0),
        Loc::Witt => ("You are at Witt's End. Passages lead off in \"all\" directions.", "You're at Witt's End.", WITT_HINT),
        Loc::Bedquilt => ("You are in Bedquilt, a long east/west passage with holes everywhere. To explore at random select north, south, up or down.","You're in Bedquilt.",0),
        Loc::Cheese => ("You are in a room whose walls resemble Swiss cheese. Obvious passages go west, east, NE and NW. Part of the room is occupied by a large bedrock block.","You're in Swiss cheese room.", 0),
        Loc::Soft  => ("You are in the Soft Room. The walls are covered with heavy curtains, the floor with a thick pile carpet. Moss covers the ceiling.", "You're in Soft Room.", 0),
        Loc::E2pit => ("You are at the east end of the Twopit Room. The floor here is littered with thin rock slabs, which make it easy to descend the pits. There is a path here bypassing the pits to connect passages from east and west. There are holes all over, but the only big one is on the wall directly over the west pit where you can't get to it.", "You're at east end of Twopit Room.", 0),
        Loc::W2pit => ("You are at the west end of the Twopit Room. There is a large hole in the wall above the pit at this end of the room.", "You're at west end of Twopit Room.", 0),
        Loc::Epit => ("You are at the bottom of the eastern pit in the Twopit Room. There is a small pool of oil in one corner of the pit.", "You're in east pit.", LIQUID+OIL),
        Loc::Wpit => ("You are at the bottom of the western pit in the Twopit Room. There is a large hole in the wall about 25 feet above you.", "You're in west pit.", 0),
        Loc::Narrow => ("You are in a long, narrow corridor stretching out of sight to the west. At the eastern end is a hole through which you can see a profusion of leaves.", "You're in a narrow corridor.", 0),
        Loc::Giant => ("You are in the Giant Room. The ceiling here is too high up for your lamp to show it. Cavernous passages lead east, north, and south. On the west wall is scrawled the inscription, \"FEE FIE FOE FOO\" [sic].", "You're in Giant Room.", 0),
        Loc::Block => ("The passage here is blocked by a recent cave-in.", "", 0),
        Loc::Immense => ("You are at one end of an immense north/south passage", "", 0),
        Loc::Falls => ("You are in a magnificent cavern with a rushing stream, which cascades over a sparkling waterfall into a roaring whirlpool that disappears through a hole in the floor. Passages exist to the south and west.", "You're in cavern with waterfall.", LIQUID),
        Loc::Steep => ("You are at the top of a steep incline above a large room. You could climb down here, but you would not be able to climb up. There is a passage leading back to the north.", "You're at steep incline above large room.", 0),
        Loc::Abovep => ("You are in a secret N/S canyon above a sizable passage.", "", 0),
        Loc::Sjunc => ("You are in a secret canyon at a junction of three canyos, bearing north, south and SE. The north one is as tall as the other two combined.", "You're at junction of three secret canyons.", 0),
        Loc::Tite => ("A large stalactite extends from the roof and almost reaches the floor below. You could climb down it, and jump from it to the floor, but having done so you would be unable to reach it to climb back up.", "You're on top of stalactite.", 0),
        Loc::Low => ("You are in a large low room. Crawls lead north, SE and SW.", "", 0),
        Loc::Crawl => ("Dead end crawl.", "", 0),
        Loc::Window => ("You're at a low window overlooking a huge pit, which extends up out of sight. A floor is indistinctly visible over 50 feet below. Traces of white mist cover the floor of the pit, becoming thicker to he left. Marks in the dust around the window would seem to indicate that someone has been here recently. Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room. A shadowy figure can be seen there peering back to you.", "You're at window on pit.", 0),
        Loc::Oriental => ("This is the Oriental Room. Ancient oriental cave drawings cover the walls. A gently sloping passage leads upward to the north, another passage leads SE, and a hands-and-knees crawl leads west.", "You're in Oriental Room.", 0),
        Loc::Misty => ("You are following a wide path around the outer edge of a large cavern. Far below, through a heavy white mist, strange splashing noises can be heard. The mist rises up through a fissure in the ceiling. The path exists to the south and west.", "You're in misty cavern.", 0),
        Loc::Alcove => ("You are in an alcove. A small NW path seems to widen after a short distance. An extremely thight tunnel leads east. It looks like a very tight squeeze. An eerie light can be seen at the other end.", "You're in alcove", DARK_HINT),
        Loc::Proom => ("You're in a small chamber lit by an eerie green light. An extremely narrow tunnel exits to the west. A dark corridor leads NE.", "You're in Plover Room.", DARK_HINT),
        Loc::Droom => ("You're in the Dark-Room. A corridor leading south is the only exit.", "You're in Dark-Room", DARK_HINT),
        Loc::Slab => ("You are in a large low circular chamber whose floor is an immense slab fallen from the ceiling (Slab Room). There once where large passages to the east and west, but they are now filled with boulders. Low small passages go north and south, and the south one quickly bends west around the boulders.", "You're in Slab Room.", 0),
        Loc::Abover => ("You are in a secret N/S canyon above a large room.", "", 0),
        Loc::Mirror => ("You are in a north/south canyon about 25 feet across. The floor is covered by white mist seeping in from the north. The walls extend upward for well over 100 feet. Suspended from some unseen point far above you, an enormous two-sided mirror is hanging parallel to and midway between the canyon walls. (The mirror is obviously provided for the use of the dwarves, who as you know are extremely vain.) A small window can be seen in either wall, some fifty feet up.", "You're in mirror canyon.", 0),
        Loc::Res => ("You are at the edge of a large underground reservoir. An opaque cloud of white mist fills the room and rises rapidly upward. The lake is fed by a stream, which tumbles out of a hole in the wall about 10 feet overhead and splashes noisily into the water somewhere within the mist. The only passage goes back toward the south.", "You're at reservoir.", LIQUID),
        Loc::Scan1 => ("You are in a secret canyon that exists to the north and east.", "", 0),
        Loc::Secret => ("You are in a secret canyon, which here runs E/W. It crosses over a very tight canyon 15 feet below. If you go down you may not be able to get back up.", "You're in secret E/W canyo above tight canyon.", 0),
        Loc::Wide => ("You are at a wide place in a very tight N/S canyon.", "", 0),
        Loc::Tight => ("The canyo here becomes too tight to go further south.", "", 0),
        Loc::Tall => ("You are in a tall E/W canyon. A low tight crawl goes 3 feet north and seems to open up.", "You're in tall E/W canyon.", 0),
        Loc::Boulders => ("The canyon runs into a mass of boulders --- dead end.", "", 0),
        Loc::Scorr => ("You are in a long winding corridor sloping out of sight in both directions.", "You're in sloping corridor.", 0),
        Loc::Swside => ("You are on one side of a large, dep chasm. A heavy white mist rising up from below obscures all view of the far side. A SW path leads away from the chasm into a winding corridor.", "You're on SW side of chasm.", 0),
        Loc::Dead0 | Loc::Dead2 | Loc::Dead8 => ("Dead end.", "", 0),
        Loc::Dead1 | Loc::Dead3 | Loc::Dead4 | Loc::Dead5 |
        Loc::Dead6 | Loc::Dead7 | Loc::Dead9 | Loc::Dead10|
        Loc::Dead11 => ("Dead end.", "", TWIST_HINT),
        Loc::Neside => ("You are on the far side of the chasm. A NE path leads away from the chasm on this side.", "You're on NE side of chasm.", 0),
        Loc::Corr => ("You're in a long east/west corridor. A faint rumbling noise can be heard in the distance.", "You're in corridor.", 0),
        Loc::Fork => ("The path forks here. The left fork leads northeast. A dull rumbling seems to get louder in that direction. The right fork leads southeast down a gentle slope. The main corridor enters from the west.", "You're at fork in path.", 0),
        Loc::Warm => ("The walls are quite warm here. From the north can be heard a steady roar, so loud that the entire cave seems to be trembling. Another passage leads south, and a low crawl goes east.", "You're at junction with warm walls.", 0),
        Loc::View => ("You are on the edge of a breath-taking view. Far below you is an active volcano, from which great gouts of molten lava come surging out, cascading back down into the depths. The glowing rock fills the farthest reaches of the cavern with a blood-red glare, giving everything an eerie macabre appearance. The air is filled with flickering sparks of ash and a heavy smell of brimstone. The walls are hot to the touch, and the thundering of the volcano drowns out all other sounds. Embedded in the jagged roof far overhead are myriad twisted formations, composed of pure white alabaster, which scatter the murky light into sinister apparitions upon the walls. To one side is a deep gorge, filled with a bizarre chaos of tortured rock that seems to have been crafted by the Devil himself. An immense river of fire crashes out from the depths of the volcano, burns its way through the gorge and plummets into a bottomless pit far off to your left. To the right, an immense geyser of blistering steam erupts continuously from barren island in the center of a sulfurous lake, which bubbles ominously. The far right wall is aflame with an incandescence of its own, which lends an additional infernal splendor to the already hellish scene. A dark, foreboding passage exits to the south.", "You're at breath-taking view", LIGHTED),

        Loc::Chamber => ("You are in a small chamber filled with large boulders. The walls are very warm, causing the air in the room to be almost stifling from the heat. The only exit is a crawl heading west, through which a low rubling noise is coming", "You're in chamber of boulders.", 0),
        Loc::Lime => ("You are walking along a gently sloping north/south passage lined with oddly shaped limestone formations.", "You're in limestone passage.", 0),
        Loc::Fbarr => ("You are standing at the entrance to a large, barren room. A sign posted above the entrance reads: \"CAUTION! BEAR IN ROOM!\"", "You're in front of barren room.", 0),
        Loc::Barr => ("You are inside a barren room. The center of the room is completely empty except for some dust. Marks in the dust lead away toward the far end of the room. The only exit is the way you came in.", "You're in barren room.", 0),
        Loc::Neend => ("You are at the northeast end of an immense room, even larger than the Giant Room. It appears to be a repository for the \"Adventure\" program. Massive torches far overhead bathe the room with smoky yellow light. Scattered about you can be seen a pile of bottles (all of them empty), a nursery of young beanstalks murmuring quietly, a bed of oysters, a bundle of black rods with rusty stars on their ends, and a collection of brass lanterns. Off to one side a great many dwarves are sleeping on the floor, snoring loudly. A sign nearby reads: \"DO NOT DISTURB THE DWARVES!\" An immense mirror is hanging against one wall, and stretches to the other end of the room, where various other sundry objects can be glimpsed dimly in the distance.", "You're at NE end.", LIGHTED),
        Loc::Swend => ("You are at the southwest end of the repository. To one side is a pit full of fierce green snakes. On the other side is a row of small wicker cages, each of which contains a little sulking bird. In one corner is a bundle of black rods with rusty marks on their ends. A large number of velvet pillows are scattered about on the floor. A vast mirror stretches off to the northeast. At your feet is a large steel grate, next to which is a sign that reads, \"TREASURE VAULT. KEYS IN MAIN OFFICE.\"", "You're at SW end.", LIGHTED),
        Loc::Crack => ("The crack is far too small for you to follow.", "", 0),
        Loc::Neck => ("You are at the bottom of the pit with a broken neck.", "", 0),
        Loc::Lose => ("You didn't make it.", "", 0),
        Loc::Cant => ("The dome is unclimable.", "", 0),
        Loc::Climb => ("You clamber up the plant and scurry through the hole at the top.", "", 0),
        Loc::Check => ("", "", 0),
        Loc::Snaked=> ("You can't get by the snake.", "", 0),
        Loc::Thru | Loc::Duck => ("You have crawled through a very low wide passage parallel to and north of the Hall of Mists.", "", 0),
        Loc::Sewer => ("The stream flows out through a pair of 1-foot-diameter sewer pipes. It would be advisable to use the exit.", "", 0),
        Loc::Upnout => ("There is nothing here to climb. Use \"up\" or \"out\" to leave the pit.", "", 0),
        Loc::Didit => ("You have climbed up the plant and out of the pit.", "", 0),
        _ => ("", "", 0),
    }
}

impl Game {
    fn lookup_instructions(&self) -> Loc {
        let dwarf = false;
        match (self.loc, self.mot) {
            (Loc::Road, Mot::W | Mot::U | Mot::Road) => Loc::Hill,
            (Loc::Road, Mot::E | Mot::In | Mot::House | Mot::Enter) => Loc::House,
            (Loc::Road, Mot::S | Mot::D | Mot::Gully | Mot::Stream | Mot::Downstream) => {
                Loc::Valley
            }
            (Loc::Road, Mot::N | Mot::Woods) => Loc::Forest,
            (Loc::Road, Mot::Depression) => Loc::Woods,

            (Loc::Hill, Mot::Road | Mot::House | Mot::Forward | Mot::E | Mot::D) => Loc::Road,
            (Loc::Hill, Mot::Woods | Mot::N | Mot::S) => Loc::Forest,

            (Loc::House, Mot::Enter | Mot::Out | Mot::Outdoors | Mot::W) => Loc::Road,
            (Loc::House, Mot::Xyzzy) => Loc::Debris,
            (Loc::House, Mot::Plugh) => Loc::Y2,
            (Loc::House, Mot::Downstream | Mot::Stream) => Loc::Sewer,

            (Loc::Valley, Mot::Upstream | Mot::House | Mot::N) => Loc::Road,
            (Loc::Valley, Mot::Woods | Mot::E | Mot::W | Mot::U) => Loc::Forest,
            (Loc::Valley, Mot::Downstream | Mot::S | Mot::D) => Loc::Slit,
            (Loc::Valley, Mot::Depression) => Loc::Outside,

            (Loc::Forest, Mot::Valley | Mot::E | Mot::D) => Loc::Valley,
            (Loc::Forest, Mot::Woods | Mot::Forward | Mot::N) => {
                if pct(50) {
                    Loc::Forest
                } else {
                    Loc::Woods
                }
            }
            (Loc::Forest, Mot::W | Mot::S) => Loc::Forest,

            (Loc::Woods, Mot::Road | Mot::N) => Loc::Road,
            (Loc::Woods, Mot::Valley | Mot::E | Mot::W | Mot::D) => Loc::Valley,
            (Loc::Woods, Mot::Woods | Mot::S) => Loc::Forest,

            (Loc::Slit, Mot::House) => Loc::Road,
            (Loc::Slit, Mot::Upstream | Mot::N) => Loc::Valley,
            (Loc::Slit, Mot::Woods | Mot::E | Mot::W) => Loc::Forest,
            (Loc::Slit, Mot::Downstream | Mot::Rock | Mot::Bed | Mot::S) => Loc::Outside,
            (Loc::Slit, Mot::Slit | Mot::Stream | Mot::D) => Loc::Sayit0,

            (Loc::Outside, Mot::Woods | Mot::E | Mot::W | Mot::S) => Loc::Forest,
            (Loc::Outside, Mot::House) => Loc::Road,
            (Loc::Outside, Mot::Upstream | Mot::Gully | Mot::N) => Loc::Slit,
            (Loc::Outside, Mot::Enter | Mot::In | Mot::D) if self.prop[Obj::Grate] != 0 => {
                Loc::Inside
            }
            (Loc::Outside, Mot::Enter | Mot::In | Mot::D) => Loc::Sayit0,

            (Loc::Inside, Mot::Out | Mot::U) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Inside, Mot::Out | Mot::U) => Loc::Sayit1,
            (Loc::Inside, Mot::Crawl | Mot::Cobbles | Mot::In | Mot::W) => Loc::Cobbles,
            (Loc::Inside, Mot::Pit) => Loc::Spit,
            (Loc::Inside, Mot::Debris) => Loc::Debris,

            (Loc::Cobbles, Mot::Out | Mot::Surface | Mot::Nowhere | Mot::E) => Loc::Inside,
            (Loc::Cobbles, Mot::In | Mot::Dark | Mot::W | Mot::Debris) => Loc::Debris,
            (Loc::Cobbles, Mot::Pit) => Loc::Spit,

            (Loc::Debris, Mot::Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Debris, Mot::Entrance) => Loc::Inside,
            (Loc::Debris, Mot::Crawl | Mot::Cobbles | Mot::Passage | Mot::Low | Mot::E) => {
                Loc::Cobbles
            }
            (Loc::Debris, Mot::Canyon | Mot::In | Mot::U | Mot::W) => Loc::Awk,
            (Loc::Debris, Mot::Xyzzy) => Loc::House,
            (Loc::Debris, Mot::Pit) => Loc::Spit,

            (Loc::Awk, Mot::Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Awk, Mot::Entrance) => Loc::Inside,
            (Loc::Awk, Mot::D | Mot::E | Mot::Debris) => Loc::Debris,
            (Loc::Awk, Mot::In | Mot::U | Mot::W) => Loc::Bird,
            (Loc::Awk, Mot::Pit) => Loc::Spit,

            (Loc::Bird, Mot::Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Bird, Mot::Entrance) => Loc::Inside,
            (Loc::Bird, Mot::Debris) => Loc::Debris,
            (Loc::Bird, Mot::Canyon | Mot::E) => Loc::Awk,
            (Loc::Bird, Mot::Passage | Mot::Pit | Mot::W) => Loc::Spit,

            (Loc::Spit, Mot::Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Spit, Mot::Entrance) => Loc::Inside,
            (Loc::Spit, Mot::Debris) => Loc::Debris,
            (Loc::Spit, Mot::Passage | Mot::E) => Loc::Bird,
            (Loc::Spit, Mot::D | Mot::Pit | Mot::Steps) if self.toting(Obj::Gold) => Loc::Neck,
            (Loc::Spit, Mot::D) if !self.toting(Obj::Gold) => Loc::Emist,
            (Loc::Spit, Mot::Crack | Mot::W) => Loc::Crack,

            (Loc::Emist, Mot::L | Mot::S) => Loc::Nugget,
            (Loc::Emist, Mot::Forward | Mot::Hall | Mot::W) => Loc::Efiss,
            (Loc::Emist, Mot::Stairs | Mot::D | Mot::N) => Loc::Hmk,
            (Loc::Emist, Mot::U | Mot::Pit | Mot::Steps | Mot::Dome | Mot::Passage | Mot::E)
                if self.toting(Obj::Gold) =>
            {
                Loc::Cant
            }
            (Loc::Emist, Mot::U) => Loc::Spit,
            (Loc::Emist, Mot::Y2) => Loc::Jumble,

            (Loc::Nugget, Mot::Hall | Mot::Out | Mot::N) => Loc::Emist,

            (Loc::Efiss, Mot::Hall | Mot::E) => Loc::Emist,

            (Loc::Efiss, Mot::Jump) if self.prop[Obj::Crystal] == 0 => Loc::Sayit2,
            (Loc::Efiss, Mot::Forward) if self.prop[Obj::Crystal] == 1 => Loc::Lose,
            (Loc::Efiss, Mot::Over | Mot::Across | Mot::W | Mot::Cross)
                if self.prop[Obj::Crystal] == 1 =>
            {
                Loc::Sayit3
            }
            (Loc::Efiss, Mot::Over) => Loc::Wfiss,

            (Loc::Wfiss, Mot::Jump) if self.prop[Obj::Crystal] == 0 => Loc::Sayit2,

            (Loc::Wfiss, Mot::Forward) if self.prop[Obj::Crystal] == 1 => Loc::Lose,
            (Loc::Wfiss, Mot::Over | Mot::Across | Mot::E | Mot::Cross)
                if self.prop[Obj::Crystal] == 1 =>
            {
                Loc::Sayit3
            }
            (Loc::Wfiss, Mot::Over) => Loc::Efiss,
            (Loc::Wfiss, Mot::N) => Loc::Thru,
            (Loc::Wfiss, Mot::W) => Loc::Wmist,

            (Loc::Wmist, Mot::S | Mot::U | Mot::Passage | Mot::Climb) => Loc::Like1,
            (Loc::Wmist, Mot::E) => Loc::Wfiss,
            (Loc::Wmist, Mot::N) => Loc::Duck,
            (Loc::Wmist, Mot::W | Mot::Crawl) => Loc::Elong,

            (Loc::Like1, Mot::U) => Loc::Wmist,
            (Loc::Like1, Mot::N) => Loc::Like1,
            (Loc::Like1, Mot::E) => Loc::Like2,
            (Loc::Like1, Mot::S) => Loc::Like4,
            (Loc::Like1, Mot::W) => Loc::Like11,

            (Loc::Like2, Mot::W) => Loc::Like1,
            (Loc::Like2, Mot::S) => Loc::Like3,
            (Loc::Like2, Mot::E) => Loc::Like4,

            (Loc::Like3, Mot::E) => Loc::Like2,
            (Loc::Like3, Mot::D) => Loc::Dead5,
            (Loc::Like3, Mot::S) => Loc::Like6,
            (Loc::Like3, Mot::N) => Loc::Dead9,

            (Loc::Like4, Mot::W) => Loc::Like1,
            (Loc::Like4, Mot::N) => Loc::Like2,
            (Loc::Like4, Mot::E) => Loc::Dead3,
            (Loc::Like4, Mot::S) => Loc::Dead4,
            (Loc::Like4, Mot::U | Mot::D) => Loc::Like14,

            (Loc::Like5, Mot::E) => Loc::Like6,
            (Loc::Like5, Mot::W) => Loc::Like7,

            (Loc::Like6, Mot::E) => Loc::Like3,
            (Loc::Like6, Mot::W) => Loc::Like5,
            (Loc::Like6, Mot::D) => Loc::Like7,
            (Loc::Like6, Mot::S) => Loc::Like8,

            (Loc::Like7, Mot::W) => Loc::Like5,
            (Loc::Like7, Mot::U) => Loc::Like6,
            (Loc::Like7, Mot::E) => Loc::Like8,
            (Loc::Like7, Mot::S) => Loc::Like9,

            (Loc::Like8, Mot::W) => Loc::Like6,
            (Loc::Like8, Mot::E) => Loc::Like7,
            (Loc::Like8, Mot::S) => Loc::Like8,
            (Loc::Like8, Mot::U) => Loc::Like9,
            (Loc::Like8, Mot::N) => Loc::Like10,
            (Loc::Like8, Mot::D) => Loc::Like11,

            (Loc::Like9, Mot::W) => Loc::Like7,
            (Loc::Like9, Mot::N) => Loc::Like8,
            (Loc::Like9, Mot::S) => Loc::Like6,

            (Loc::Like10, Mot::W) => Loc::Like8,
            (Loc::Like10, Mot::N) => Loc::Like10,
            (Loc::Like10, Mot::D) => Loc::Dead7,
            (Loc::Like10, Mot::E) => Loc::Brink,

            (Loc::Like11, Mot::N) => Loc::Like1,
            (Loc::Like11, Mot::W | Mot::S) => Loc::Like11,
            (Loc::Like11, Mot::E) => Loc::Dead1,

            (Loc::Like12, Mot::S) => Loc::Brink,
            (Loc::Like12, Mot::E) => Loc::Like13,
            (Loc::Like12, Mot::W) => Loc::Dead10,

            (Loc::Like13, Mot::N) => Loc::Brink,
            (Loc::Like13, Mot::W) => Loc::Like12,
            (Loc::Like13, Mot::NW) => Loc::Dead2, // dk - a dirty trick!

            (Loc::Like14, Mot::U | Mot::D) => Loc::Like4,

            (Loc::Brink, Mot::D | Mot::Climb) => Loc::Bird,
            (Loc::Brink, Mot::W) => Loc::Like10,
            (Loc::Brink, Mot::S) => Loc::Dead8,
            (Loc::Brink, Mot::N) => Loc::Like12,
            (Loc::Brink, Mot::E) => Loc::Like13,

            (Loc::Elong, Mot::E | Mot::U | Mot::Crawl | Mot::W) => Loc::Wlong,
            (Loc::Elong, Mot::N | Mot::D | Mot::Hole) => Loc::Cross,

            (Loc::Wlong, Mot::E) => Loc::Elong,
            (Loc::Wlong, Mot::N) => Loc::Cross,
            (Loc::Wlong, Mot::S) if !dwarf => Loc::Diff0,

            (Loc::Diff0, Mot::S) => Loc::Diff1,
            (Loc::Diff0, Mot::SW) => Loc::Diff2,
            (Loc::Diff0, Mot::NE) => Loc::Diff3,
            (Loc::Diff0, Mot::SE) => Loc::Diff4,
            (Loc::Diff0, Mot::U) => Loc::Diff5,
            (Loc::Diff0, Mot::NW) => Loc::Diff6,
            (Loc::Diff0, Mot::E) => Loc::Diff7,
            (Loc::Diff0, Mot::W) => Loc::Diff8,
            (Loc::Diff0, Mot::N) => Loc::Diff9,
            (Loc::Diff0, Mot::D) => Loc::Wlong,

            (Loc::Diff1, Mot::W) => Loc::Diff0,
            (Loc::Diff1, Mot::SE) => Loc::Diff2,
            (Loc::Diff1, Mot::NW) => Loc::Diff3,
            (Loc::Diff1, Mot::SW) => Loc::Diff4,
            (Loc::Diff1, Mot::NE) => Loc::Diff5,
            (Loc::Diff1, Mot::U) => Loc::Diff6,
            (Loc::Diff1, Mot::D) => Loc::Diff7,
            (Loc::Diff1, Mot::N) => Loc::Diff8,
            (Loc::Diff1, Mot::S) => Loc::Diff9,
            (Loc::Diff1, Mot::E) => Loc::Diff10,

            (Loc::Diff2, Mot::NW) => Loc::Diff0,
            (Loc::Diff2, Mot::U) => Loc::Diff1,
            (Loc::Diff2, Mot::N) => Loc::Diff3,
            (Loc::Diff2, Mot::S) => Loc::Diff4,
            (Loc::Diff2, Mot::W) => Loc::Diff5,
            (Loc::Diff2, Mot::SW) => Loc::Diff6,
            (Loc::Diff2, Mot::NE) => Loc::Diff7,
            (Loc::Diff2, Mot::E) => Loc::Diff8,
            (Loc::Diff2, Mot::D) => Loc::Diff9,
            (Loc::Diff2, Mot::SE) => Loc::Diff10,

            (Loc::Diff3, Mot::U) => Loc::Diff0,
            (Loc::Diff3, Mot::D) => Loc::Diff1,
            (Loc::Diff3, Mot::W) => Loc::Diff2,
            (Loc::Diff3, Mot::NE) => Loc::Diff4,
            (Loc::Diff3, Mot::SW) => Loc::Diff5,
            (Loc::Diff3, Mot::E) => Loc::Diff6,
            (Loc::Diff3, Mot::N) => Loc::Diff7,
            (Loc::Diff3, Mot::NW) => Loc::Diff8,
            (Loc::Diff3, Mot::SE) => Loc::Diff9,
            (Loc::Diff3, Mot::S) => Loc::Diff10,

            (Loc::Diff4, Mot::NE) => Loc::Diff0,
            (Loc::Diff4, Mot::N) => Loc::Diff1,
            (Loc::Diff4, Mot::NW) => Loc::Diff2,
            (Loc::Diff4, Mot::SE) => Loc::Diff3,
            (Loc::Diff4, Mot::E) => Loc::Diff5,
            (Loc::Diff4, Mot::D) => Loc::Diff6,
            (Loc::Diff4, Mot::S) => Loc::Diff7,
            (Loc::Diff4, Mot::U) => Loc::Diff8,
            (Loc::Diff4, Mot::W) => Loc::Diff9,
            (Loc::Diff4, Mot::SW) => Loc::Diff10,

            (Loc::Diff5, Mot::N) => Loc::Diff0,
            (Loc::Diff5, Mot::SE) => Loc::Diff1,
            (Loc::Diff5, Mot::D) => Loc::Diff2,
            (Loc::Diff5, Mot::S) => Loc::Diff3,
            (Loc::Diff5, Mot::E) => Loc::Diff4,
            (Loc::Diff5, Mot::W) => Loc::Diff6,
            (Loc::Diff5, Mot::SW) => Loc::Diff7,
            (Loc::Diff5, Mot::NE) => Loc::Diff8,
            (Loc::Diff5, Mot::NW) => Loc::Diff9,
            (Loc::Diff5, Mot::U) => Loc::Diff10,

            (Loc::Diff6, Mot::E) => Loc::Diff0,
            (Loc::Diff6, Mot::W) => Loc::Diff1,
            (Loc::Diff6, Mot::U) => Loc::Diff2,
            (Loc::Diff6, Mot::SW) => Loc::Diff3,
            (Loc::Diff6, Mot::D) => Loc::Diff4,
            (Loc::Diff6, Mot::S) => Loc::Diff5,
            (Loc::Diff6, Mot::NW) => Loc::Diff7,
            (Loc::Diff6, Mot::SE) => Loc::Diff8,
            (Loc::Diff6, Mot::NE) => Loc::Diff9,
            (Loc::Diff6, Mot::N) => Loc::Diff10,

            (Loc::Diff7, Mot::SE) => Loc::Diff0,
            (Loc::Diff7, Mot::NE) => Loc::Diff1,
            (Loc::Diff7, Mot::S) => Loc::Diff2,
            (Loc::Diff7, Mot::D) => Loc::Diff3,
            (Loc::Diff7, Mot::U) => Loc::Diff4,
            (Loc::Diff7, Mot::NW) => Loc::Diff5,
            (Loc::Diff7, Mot::N) => Loc::Diff6,
            (Loc::Diff7, Mot::SW) => Loc::Diff8,
            (Loc::Diff7, Mot::E) => Loc::Diff9,
            (Loc::Diff7, Mot::W) => Loc::Diff10,

            (Loc::Diff8, Mot::D) => Loc::Diff0,
            (Loc::Diff8, Mot::E) => Loc::Diff1,
            (Loc::Diff8, Mot::NE) => Loc::Diff2,
            (Loc::Diff8, Mot::U) => Loc::Diff3,
            (Loc::Diff8, Mot::W) => Loc::Diff4,
            (Loc::Diff8, Mot::N) => Loc::Diff5,
            (Loc::Diff8, Mot::S) => Loc::Diff6,
            (Loc::Diff8, Mot::SE) => Loc::Diff7,
            (Loc::Diff8, Mot::SW) => Loc::Diff9,
            (Loc::Diff8, Mot::NW) => Loc::Diff10,

            (Loc::Diff9, Mot::SW) => Loc::Diff0,
            (Loc::Diff9, Mot::NW) => Loc::Diff1,
            (Loc::Diff9, Mot::E) => Loc::Diff2,
            (Loc::Diff9, Mot::W) => Loc::Diff3,
            (Loc::Diff9, Mot::N) => Loc::Diff4,
            (Loc::Diff9, Mot::D) => Loc::Diff5,
            (Loc::Diff9, Mot::SE) => Loc::Diff6,
            (Loc::Diff9, Mot::U) => Loc::Diff7,
            (Loc::Diff9, Mot::S) => Loc::Diff8,
            (Loc::Diff9, Mot::NE) => Loc::Diff10,

            (Loc::Diff10, Mot::SW) => Loc::Diff1,
            (Loc::Diff10, Mot::N) => Loc::Diff2,
            (Loc::Diff10, Mot::E) => Loc::Diff3,
            (Loc::Diff10, Mot::NW) => Loc::Diff4,
            (Loc::Diff10, Mot::SE) => Loc::Diff5,
            (Loc::Diff10, Mot::NE) => Loc::Diff6,
            (Loc::Diff10, Mot::W) => Loc::Diff7,
            (Loc::Diff10, Mot::D) => Loc::Diff8,
            (Loc::Diff10, Mot::U) => Loc::Diff9,
            (Loc::Diff10, Mot::S) => Loc::Pony,

            (Loc::Pony, Mot::N | Mot::Out) => Loc::Diff10,

            (Loc::Cross, Mot::W) => Loc::Elong,
            (Loc::Cross, Mot::N) => Loc::Dead0,
            (Loc::Cross, Mot::E) => Loc::West,
            (Loc::Cross, Mot::S) => Loc::Wlong,

            (Loc::Hmk, Mot::Stairs | Mot::U | Mot::E) => Loc::Emist,
            (Loc::Hmk, Mot::N | Mot::L) if self.prop[Obj::Snake] != 0 => Loc::Ns,
            (Loc::Hmk, Mot::S | Mot::R) if self.prop[Obj::Snake] != 0 => Loc::South,
            (Loc::Hmk, Mot::W | Mot::Forward) if self.prop[Obj::Snake] != 0 => Loc::West,
            (Loc::Hmk, Mot::N) => Loc::Snaked,
            (Loc::Hmk, Mot::SW) if pct(35) => Loc::Secret,
            (Loc::Hmk, Mot::SW) if self.is_here(Obj::Snake) => Loc::Snaked,
            (Loc::Hmk, Mot::Secret) => Loc::Secret,

            (Loc::West, Mot::Hall | Mot::Out | Mot::E) => Loc::Hmk,
            (Loc::West, Mot::W | Mot::U) => Loc::Cross,

            (Loc::South, Mot::Hall | Mot::Out | Mot::N) => Loc::Hmk,

            (Loc::Ns, Mot::Hall | Mot::Out | Mot::S) => Loc::Hmk,
            (Loc::Ns, Mot::N | Mot::Y2) => Loc::Y2,
            (Loc::Ns, Mot::D | Mot::Hole) => Loc::Dirty,

            (Loc::Y2, Mot::Plugh) => Loc::House,
            (Loc::Y2, Mot::S) => Loc::Ns,
            (Loc::Y2, Mot::E | Mot::Wall | Mot::Broken) => Loc::Jumble,
            (Loc::Y2, Mot::W) => Loc::Windoe,
            (Loc::Y2, Mot::Plover) if self.toting(Obj::Emerald) => Loc::Pdrop,
            (Loc::Y2, Mot::Plover) => Loc::Proom,

            (Loc::Jumble, Mot::D) => Loc::Y2,
            (Loc::Jumble, Mot::U) => Loc::Emist,

            (Loc::Windoe, Mot::E | Mot::Y2) => Loc::Y2,
            (Loc::Windoe, Mot::Jump) => Loc::Neck,

            (Loc::Dirty, Mot::E | Mot::Crawl) => Loc::Clean,
            (Loc::Dirty, Mot::U | Mot::Hole) => Loc::Ns,
            (Loc::Dirty, Mot::W) => Loc::Dusty,
            (Loc::Dirty, Mot::Bedquilt) => Loc::Bedquilt,

            (Loc::Clean, Mot::W | Mot::Crawl) => Loc::Dirty,
            (Loc::Clean, Mot::D | Mot::Pit | Mot::Climb) => Loc::Wet,

            (Loc::Wet, Mot::Climb | Mot::U | Mot::Out) => Loc::Clean,
            (Loc::Wet, Mot::Slit | Mot::Stream | Mot::D | Mot::Upstream | Mot::Downstream) => {
                Loc::Sayit0
            }

            (Loc::Dusty, Mot::E | Mot::Passage) => Loc::Dirty,
            (Loc::Dusty, Mot::D | Mot::Hole | Mot::Floor) => Loc::Complex,
            (Loc::Dusty, Mot::Bedquilt) => Loc::Bedquilt,

            (Loc::Complex, Mot::U | Mot::Climb | Mot::Room) => Loc::Dusty,
            (Loc::Complex, Mot::W | Mot::Bedquilt) => Loc::Bedquilt,
            (Loc::Complex, Mot::N | Mot::Shell) => Loc::Shell,
            (Loc::Complex, Mot::E) => Loc::Ante,

            (Loc::Shell, Mot::U | Mot::Hall) => Loc::Arch,
            (Loc::Shell, Mot::D) => Loc::Ragged,
            (Loc::Shell, Mot::S) if self.toting(Obj::Clam) => Loc::Sayit4,

            (Loc::Shell, Mot::S) if self.toting(Obj::Oyster) => Loc::Sayit5,

            (Loc::Shell, Mot::S) => Loc::Complex,

            (Loc::Arch, Mot::D | Mot::Shell | Mot::Out) => Loc::Shell,

            (Loc::Ragged, Mot::U | Mot::Shell) => Loc::Shell,
            (Loc::Ragged, Mot::D) => Loc::Sac,

            (Loc::Sac, Mot::U | Mot::Out) => Loc::Ragged,
            (Loc::Sac, Mot::Shell) => Loc::Shell,

            (Loc::Ante, Mot::U) => Loc::Complex,
            (Loc::Ante, Mot::W) => Loc::Bedquilt,
            (Loc::Ante, Mot::E) => Loc::Witt,

            (
                Loc::Witt,
                Mot::E | Mot::N | Mot::S | Mot::NE | Mot::SE | Mot::SW | Mot::NW | Mot::U | Mot::D,
            ) if pct(95) => Loc::Sayit6,
            (Loc::Witt, Mot::E) => Loc::Ante, // one chance in 20
            (Loc::Witt, Mot::W) => Loc::Sayit6,

            (Loc::Bedquilt, Mot::E) => Loc::Complex,
            (Loc::Bedquilt, Mot::W) => Loc::Cheese,
            (Loc::Bedquilt, Mot::S) if pct(80) => Loc::Sayit6,
            (Loc::Bedquilt, Mot::Slab) => Loc::Slab,
            (Loc::Bedquilt, Mot::U) if pct(50) => Loc::Abovep,
            (Loc::Bedquilt, Mot::U) => Loc::Dusty,
            (Loc::Bedquilt, Mot::N) if pct(60) => Loc::Sayit6,
            (Loc::Bedquilt, Mot::N) if pct(75) => Loc::Low,
            (Loc::Bedquilt, Mot::N) => Loc::Sjunc,
            (Loc::Bedquilt, Mot::D) if pct(80) => Loc::Sayit6,
            (Loc::Bedquilt, Mot::D) => Loc::Ante,

            (Loc::Cheese, Mot::NE) => Loc::Bedquilt,
            (Loc::Cheese, Mot::W) => Loc::E2pit,
            (Loc::Cheese, Mot::S) if pct(80) => Loc::Sayit6,
            (Loc::Cheese, Mot::Canyon) => Loc::Tall,
            (Loc::Cheese, Mot::E) => Loc::Soft,
            (Loc::Cheese, Mot::NW) if pct(50) => Loc::Sayit6,
            (Loc::Cheese, Mot::Oriental) => Loc::Oriental,

            (Loc::Soft, Mot::W | Mot::Out) => Loc::Cheese,

            (Loc::E2pit, Mot::E) => Loc::Cheese,
            (Loc::E2pit, Mot::W | Mot::Across) => Loc::W2pit,
            (Loc::E2pit, Mot::D | Mot::Pit) => Loc::Epit,

            (Loc::W2pit, Mot::E) => Loc::E2pit,
            (Loc::W2pit, Mot::W) => Loc::Slab,
            (Loc::W2pit, Mot::D) => Loc::Wpit,
            (Loc::W2pit, Mot::Hole) => Loc::Sayit7,

            (Loc::Epit, Mot::U | Mot::Out) => Loc::E2pit,

            (Loc::Wpit, Mot::U | Mot::Out) => Loc::W2pit,
            (Loc::Wpit, Mot::Climb) if self.prop[Obj::Plant] != 4 => Loc::Check,
            (Loc::Wpit, Mot::Climb) => Loc::Climb,

            (Loc::Narrow, Mot::D | Mot::Climb) => Loc::Wpit,
            (Loc::Narrow, Mot::Jump) => Loc::Neck,
            (Loc::Narrow, Mot::W | Mot::Giant) => Loc::Giant,

            (Loc::Giant, Mot::S) => Loc::Narrow,
            (Loc::Giant, Mot::E) => Loc::Block,
            (Loc::Giant, Mot::N) => Loc::Immense,

            (Loc::Block, Mot::S | Mot::Giant | Mot::Out) => Loc::Giant,

            (Loc::Immense, Mot::S | Mot::Giant | Mot::Passage) => Loc::Giant,
            (Loc::Immense, Mot::N | Mot::Enter | Mot::Cavern) if self.prop[Obj::Door] != 0 => {
                Loc::Falls
            }

            (Loc::Falls, Mot::S | Mot::Out) => Loc::Immense,
            (Loc::Falls, Mot::Giant) => Loc::Giant,
            (Loc::Falls, Mot::W) => Loc::Steep,

            (Loc::Steep, Mot::N | Mot::Cavern | Mot::Passage) => Loc::Falls,
            (Loc::Steep, Mot::D | Mot::Climb) => Loc::Low,

            (Loc::Abovep, Mot::N) => Loc::Sjunc,
            (Loc::Abovep, Mot::D | Mot::Passage) => Loc::Bedquilt,
            (Loc::Abovep, Mot::S) => Loc::Tite,

            (Loc::Sjunc, Mot::SE) => Loc::Bedquilt,
            (Loc::Sjunc, Mot::S) => Loc::Abovep,
            (Loc::Sjunc, Mot::N) => Loc::Window,

            (Loc::Tite, Mot::N) => Loc::Abovep,
            (Loc::Tite, Mot::D | Mot::Jump | Mot::Climb) if pct(40) => Loc::Like6,
            (Loc::Tite, Mot::D | Mot::Jump | Mot::Climb) if pct(50) => Loc::Like9,
            (Loc::Tite, Mot::D | Mot::Jump | Mot::Climb) => Loc::Like4,

            (Loc::Low, Mot::Bedquilt) => Loc::Bedquilt,
            (Loc::Low, Mot::SW) => Loc::Scorr,
            (Loc::Low, Mot::N) => Loc::Crawl,
            (Loc::Low, Mot::SE | Mot::Oriental) => Loc::Oriental,

            (Loc::Crawl, Mot::S | Mot::Crawl | Mot::Out) => Loc::Low,

            (Loc::Window, Mot::W) => Loc::Sjunc,
            (Loc::Window, Mot::Jump) => Loc::Neck,

            (Loc::Oriental, Mot::SE) => Loc::Cheese,
            (Loc::Oriental, Mot::W | Mot::Crawl) => Loc::Low,
            (Loc::Oriental, Mot::U | Mot::N | Mot::Cavern) => Loc::Misty,

            (Loc::Misty, Mot::S | Mot::Oriental) => Loc::Oriental,
            (Loc::Misty, Mot::W) => Loc::Alcove,

            (Loc::Alcove, Mot::NW | Mot::Cavern) => Loc::Misty,
            (Loc::Alcove, Mot::E | Mot::Passage) => Loc::Ppass,
            (Loc::Alcove, Mot::E) => Loc::Proom, // never performed, but seen by ‘go back’

            (Loc::Proom, Mot::W | Mot::Passage | Mot::Out) => Loc::Ppass,
            (Loc::Proom, Mot::W) => Loc::Alcove, // never performed, but seen by ‘go back’
            (Loc::Proom, Mot::Plover) if self.toting(Obj::Emerald) => Loc::Pdrop,
            (Loc::Proom, Mot::Plover) => Loc::Y2,
            (Loc::Proom, Mot::NE | Mot::Dark) => Loc::Droom,

            (Loc::Droom, Mot::S | Mot::Plover | Mot::Out) => Loc::Proom,

            (Loc::Slab, Mot::S) => Loc::W2pit,
            (Loc::Slab, Mot::U | Mot::Climb) => Loc::Abover,
            (Loc::Slab, Mot::N) => Loc::Bedquilt,

            (Loc::Abover, Mot::D | Mot::Slab) => Loc::Slab,
            (Loc::Abover, Mot::S) if self.prop[Obj::Dragon] != 0 => Loc::Scan2,
            (Loc::Abover, Mot::S) => Loc::Scan1,
            (Loc::Abover, Mot::N) => Loc::Mirror,
            (Loc::Abover, Mot::Reservoir) => Loc::Res,

            (Loc::Mirror, Mot::S) => Loc::Abover,
            (Loc::Mirror, Mot::N | Mot::Reservoir) => Loc::Res,

            (Loc::Res, Mot::S | Mot::Out) => Loc::Mirror,

            (Loc::Scan1, Mot::N | Mot::Out) => Loc::Abover,

            (Loc::Immense, Mot::N | Mot::Enter | Mot::Cavern) => Loc::Sayit8,
            (Loc::Scan1, Mot::E | Mot::Forward) => Loc::Sayit9,

            (Loc::Scan2, Mot::N) => Loc::Abover,
            (Loc::Scan2, Mot::E) => Loc::Secret,

            (Loc::Scan3, Mot::E | Mot::Out) => Loc::Secret,
            (Loc::Scan3, Mot::N | Mot::Forward) => Loc::Sayit9,

            (Loc::Secret, Mot::E) => Loc::Hmk,
            (Loc::Secret, Mot::W) if self.prop[Obj::Dragon] != 0 => Loc::Scan2,
            (Loc::Secret, Mot::W) => Loc::Scan3,
            (Loc::Secret, Mot::D) => Loc::Wide,

            (Loc::Wide, Mot::S) => Loc::Tight,
            (Loc::Wide, Mot::N) => Loc::Tall,

            (Loc::Tight, Mot::N) => Loc::Wide,

            (Loc::Tall, Mot::E) => Loc::Wide,
            (Loc::Tall, Mot::W) => Loc::Boulders,
            (Loc::Tall, Mot::N | Mot::Crawl) => Loc::Cheese,

            (Loc::Boulders, Mot::S) => Loc::Tall,

            (Loc::Scorr, Mot::D) => Loc::Low,
            (Loc::Scorr, Mot::U) => Loc::Swside,

            (Loc::Swside, Mot::SW) => Loc::Scorr,
            (Loc::Swside, Mot::Over | Mot::Across | Mot::Cross | Mot::NE)
                if self.is_here(Obj::Troll) =>
            {
                Loc::Sayit10
            }
            (Loc::Swside, Mot::Over) if self.prop[Obj::Troll] != 0 => Loc::Sayit11,
            (Loc::Swside, Mot::Over) => Loc::Troll,
            (Loc::Swside, Mot::Jump) if self.prop[Obj::Troll] != 0 => Loc::Lose,
            (Loc::Swside, Mot::Jump) => Loc::Sayit2,

            (Loc::Dead0, Mot::S | Mot::Out) => Loc::Cross,
            (Loc::Dead1, Mot::W | Mot::Out) => Loc::Like11,
            (Loc::Dead2, Mot::SE) => Loc::Like13,
            (Loc::Dead3, Mot::W | Mot::Out) => Loc::Like4,
            (Loc::Dead4, Mot::E | Mot::Out) => Loc::Like4,
            (Loc::Dead5, Mot::U | Mot::Out) => Loc::Like3,
            (Loc::Dead6, Mot::W | Mot::Out) => Loc::Like9,
            (Loc::Dead7, Mot::U | Mot::Out) => Loc::Like10,
            (Loc::Dead8, Mot::E | Mot::Out) => Loc::Brink,
            (Loc::Dead9, Mot::S | Mot::Out) => Loc::Like3,
            (Loc::Dead10, Mot::E | Mot::Out) => Loc::Like12,
            (Loc::Dead11, Mot::U | Mot::Out) => Loc::Like8,

            (Loc::Neside, Mot::NE) => Loc::Corr,
            (Loc::Neside, Mot::Over | Mot::Across | Mot::Cross | Mot::SW)
                if self.is_here(Obj::Troll) =>
            {
                Loc::Sayit10
            }
            (Loc::Neside, Mot::Over) if self.prop[Obj::Troll] != 0 => Loc::Sayit11,
            (Loc::Neside, Mot::Over) => Loc::Troll,
            (Loc::Neside, Mot::Jump) => Loc::Sayit2,
            (Loc::Neside, Mot::Fork) => Loc::Fork,
            (Loc::Neside, Mot::View) => Loc::View,
            (Loc::Neside, Mot::Barren) => Loc::Fbarr,

            (Loc::Corr, Mot::W) => Loc::Neside,
            (Loc::Corr, Mot::E | Mot::Fork) => Loc::Fork,
            (Loc::Corr, Mot::View) => Loc::View,
            (Loc::Corr, Mot::Barren) => Loc::Fbarr,

            (Loc::Fork, Mot::W) => Loc::Corr,
            (Loc::Fork, Mot::NE | Mot::L) => Loc::Warm,
            (Loc::Fork, Mot::SE | Mot::R | Mot::D) => Loc::Lime,
            (Loc::Fork, Mot::View) => Loc::View,
            (Loc::Fork, Mot::Barren) => Loc::Fbarr,

            (Loc::Warm, Mot::S | Mot::Fork) => Loc::Fork,
            (Loc::Warm, Mot::N | Mot::View) => Loc::View,
            (Loc::Warm, Mot::E | Mot::Crawl) => Loc::Chamber,
            (Loc::View, Mot::S | Mot::Passage | Mot::Out) => Loc::Warm,
            (Loc::View, Mot::Fork) => Loc::Fork,
            (Loc::View, Mot::D | Mot::Jump) => Loc::Sayit12,

            (Loc::Chamber, Mot::W | Mot::Out | Mot::Crawl) => Loc::Warm,
            (Loc::Chamber, Mot::Fork) => Loc::Fork,
            (Loc::Chamber, Mot::View) => Loc::View,

            (Loc::Lime, Mot::N | Mot::U | Mot::Fork) => Loc::Fork,
            (Loc::Lime, Mot::S | Mot::D | Mot::Barren) => Loc::Fbarr,
            (Loc::Lime, Mot::View) => Loc::View,
            (Loc::Fbarr, Mot::W | Mot::U) => Loc::Lime,
            (Loc::Fbarr, Mot::Fork) => Loc::Fork,
            (Loc::Fbarr, Mot::E | Mot::In | Mot::Barren | Mot::Enter) => Loc::Barr,
            (Loc::Fbarr, Mot::View) => Loc::View,

            (Loc::Barr, Mot::W | Mot::Out) => Loc::Fbarr,
            (Loc::Barr, Mot::Fork) => Loc::Fork,
            (Loc::Barr, Mot::View) => Loc::View,

            (Loc::Neend, Mot::SW | Mot::Out) => Loc::Swend,

            (Loc::Swend, Mot::NE) => Loc::Neend,
            (Loc::Swend, Mot::D) => Loc::Sayit1,

            (Loc::Crack, _) => Loc::Spit,
            (Loc::Neck, _) => Loc::Limbo,
            (Loc::Lose, _) => Loc::Limbo,
            (Loc::Cant, _) => Loc::Emist,
            (Loc::Climb, _) => Loc::Narrow,
            (Loc::Check, _) if self.prop[Obj::Plant] != 2 => Loc::Upnout,
            (Loc::Check, _) => Loc::Didit,
            (Loc::Snaked, _) => Loc::Hmk,
            (Loc::Thru, _) => Loc::Wmist,
            (Loc::Duck, _) => Loc::Wfiss,
            (Loc::Sewer, _) => Loc::House,
            (Loc::Upnout, _) => Loc::Wpit,
            (Loc::Didit, _) => Loc::W2pit,
            _ => Loc::Nowhere,
        }
    } // end lookup_instructions
}

fn object_name(obj: Obj) -> &'static str {
    match obj {
        Obj::Rug => "Persian rug",
        Obj::Chain => "Golden chain",
        Obj::Spices => "Rare spices",
        Obj::Pearl => "Glistening pearl",
        Obj::Pyramid => "Platinum pyramid",
        Obj::Emerald => "Egg-sized emerald",
        Obj::Vase => "Ming vase",
        Obj::Trident => "Jeweled trident",
        Obj::Eggs => "Golden eggs",
        Obj::Chest => "Treasure chest",
        Obj::Coins => "Rare coins",
        Obj::Jewels => "Precious jewelry",
        Obj::Silver => "Bars of silver",
        Obj::Diamonds => "Several diamonds",
        Obj::Gold => "Large gold nugget",
        Obj::Batteries => "Batteries",
        Obj::Axe => "Dwarf's axe",
        Obj::Oil => "Oil in the bottle",
        Obj::Water => "Water in the bottle",
        Obj::Bottle => "Small bottle.",
        Obj::Food => "Tasty food.",
        Obj::Mag => "Spelunker Today",
        Obj::Oyster => "Giant oyster >GROAN!<",
        Obj::Clam => "Giant clam >GRUNT!<",
        Obj::Pillow => "Velvet pillow",
        Obj::Bird => "Little bird in a cage",
        Obj::Rod2 => "Black rod",
        Obj::Rod => "Black rod",
        Obj::Cage => "Wicker cage",
        Obj::Lamp => "Brass lantern",
        Obj::Keys => "Set of keys",
        _ => "",
    }
}

const fn object_notes(obj: Obj, prop: i8) -> &'static str {
    match (obj,prop) {
        (Obj::Rug,0) => "There is a Persian rug spread out on the floor!",
        (Obj::Rug,1) => "The dragon is sprawled out on a Persian rug!!",
        (Obj::Troll2,0) => "The troll is nowhere to be seen.",
        (Obj::Troll,0) => "A burly troll stands by the bridge and insists you throw him a treasure before you may cross.",
        (Obj::Troll,1) => "The troll steps out from beneath the bridge and blocks your way.",
        (Obj::Bridge,0) => "A rickety wooden bridge extends across the chasm, vanishing into the mist. A sign posted on the bridge reads, \"STOP! PAY TROLL!\"",
        (Obj::Bridge,1) => "The wreckage of a bridge (and a dead bear) can be seen at the bottom of the chasm.",
        (Obj::Dragon,0) => "A huge green fierce dragon bars the way!",
        (Obj::Dragon,1) => "Congratulations! You have just vanquished a dragon with your bare hands! (Unbelievable, isn't it?)",
        (Obj::Dragon,2) => "The body of a huge green dead dragon is lying off to one side.",
        (Obj::Shadow,0) => "The shadowy figure seems to be trying to attract your attention.",
        (Obj::Plant2,0) => "The top of a 12-foot-tall beanstalk is poking out of the west pit.",
        (Obj::Plant2,1) => "There is a huge beanstalk growing out of the west pit up to the hold.",
        (Obj::Crystal,0) => "A crystal bridge now spans the fissure.",
        (Obj::Crystal,1) => "The crystal bridge has vanished!",
        (Obj::Treads,0) => "Rough stone steps lead down the pit.",
        (Obj::Treads,1) => "Rough stone steps lead up the dome.",
        (Obj::Grate,0) => "The grate is locked.",
        (Obj::Grate,1) => "The grate is open.",
        (Obj::Chain,0) => "There is a golden chain lying in a heap on the floor!",
        (Obj::Chain,1) => "The bear is locked to the wall with a golden chain!",
        (Obj::Chain,2) => "There is a golden chain locked to the wall!",
        (Obj::Spices,0) => "There are rare spices here!",
        (Obj::Pearl,0) => "Off to one side lies a glistening pearl!",
        (Obj::Pyramid,0) => "There is a platinum pyramid here, 8 inches on a side!",
        (Obj::Emerald,0) => "There is an emerald here the size of a plover's egg!",
        (Obj::Vase,0) => "There is a delicate, precious, Ming vase here!",
        (Obj::Vase,1) => "The vase is now resting, delicately, on a velvet pillow.",
        (Obj::Vase,2) => "The floor is littered with worthless shards of pottery.",
        (Obj::Vase,3) => "The Ming vase drops with a delicate crash.",
        (Obj::Trident,0) => "There is a jewel-encrusted trident here!",
        (Obj::Eggs,0) => "There is a large nest here, full of golden eggs!",
        (Obj::Eggs,1) => "The nest of golden eggs has vanished!",
        (Obj::Eggs,2) => "Done!",
        (Obj::Chest,0) => "The pirate's treasure chest is here!",
        (Obj::Coins,0) => "There are many coins here!",
        (Obj::Jewels,0) => "There is precious jewelry here!",
        (Obj::Silver,0) => "There are bars of silver here",
        (Obj::Diamonds,0) => "There are diamonds here!",
        (Obj::Gold,0) => "There is a large sparkling nugget of gold here!",
        (Obj::Batteries,0) => "There are fresh batteries here.",
        (Obj::Batteries,1) => "Some worn-out batteries have been discarded nearby.",
        (Obj::Pony,0) => "There is a massive vending machine here. The instructions on it read: Drop coins here to receive fresh batteries.",
        (Obj::Message,0) => "There is a message scrawled in the dust in a flowery script, reading: \"This is not the maze where the pirate hides his treasure chest.\"",
        (Obj::Bear,0) => "There is a ferocious cave bear eying you from the far end of the room!",
        (Obj::Bear,1) => "There is a gentle cave bear sitting placidly in one corner",
        (Obj::Bear,2) => "There is a contented-looking bear wandering about nearby",
        (Obj::Axe,0) => "There is a little axe here.",
        (Obj::Axe,1) => "There is a little axe lying beside the bear.",
        (Obj::Plant,0) => "There is a tiny little plant in the pit, murmuring \'Water, water, ...\"",
        (Obj::Plant,1) => "The plant spurts into furious growth for a few seconds.",
        (Obj::Plant,2) => "There is a 12-foot-tall beanstalk stretching up out of the pit, bellowing \"Water!! Water!!\"",
        (Obj::Plant,3) => "The plant grows explosively, almost filling the bottom of the pit.",
        (Obj::Plant,4) => "There is a gigantic beanstalk stretching all the way up to the hole",
        (Obj::Plant,5) => "You've over-watered the plant! It's shriveling up! It's it's...",
        (Obj::Bottle,0) => "There is a bottle of water here.",
        (Obj::Bottle,1) => "There is an empty bottle here.",
        (Obj::Bottle,2) => "There is a bottle of oil here.",
        (Obj::Food,0) => "There is food here.",
        (Obj::Mag,0) => "There are a few recent issues of \"Spelunker Today\" magazine here.",
        (Obj::Oyster,0) => "There is an enormous oyster here with its shell tightly closed.",
        (Obj::Oyster,1) => "Interesting. There seems to be something written on the underside of the oyster.",
        (Obj::Clam,0) => "There is an enourmous clam here with its shell tightly closed",
        (Obj::Tablet,0) => "A massive stone tablet embedded in the wall reads: \n\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\"",
        (Obj::Snake,0) => "A huge green fierce snake bars the way!",
        (Obj::Pillow,0) => "A small velvet pillow lies on the floor.",
        (Obj::Door,0) => "The way north is barred by a massive, rusty, iron door.",
        (Obj::Door,1) => "The way north leads through a massive, rusty, iron door",
        (Obj::Bird,0) => "A cheerful little bird is sitting here singing.",
        (Obj::Bird,1) => "There is a little bird in the cage.",
        (Obj::Rod2,0) => "A three-foot black rod with a rusty mark on an end lies nearby.",
        (Obj::Rod,0) => "A three-foot black rod with a rusty star on an end lies nearby",
        (Obj::Cage,0) => "There is a small wicker cage discarded nearby",
        (Obj::Lamp,0) => "There is a shiny brass lamp nearby.",
        (Obj::Lamp,1) => "There is a lamp shining nearby.",
        (Obj::Keys,0) => "There are some keys on the ground here.",
        _ => "",
    }
}

fn is_forced(loc: Loc) -> bool {
    loc >= Loc::Crack && loc <= Loc::Troll
}

const fn init_movable() -> [bool; N_OBJECTS] {
    #[rustfmt::skip]
    let l=[Obj::Gold, Obj::Diamonds, Obj::Silver, Obj::Jewels,
     Obj::Coins, Obj::Chest, Obj::Eggs, Obj::Trident,
     Obj::Vase, Obj::Emerald, Obj::Pyramid,Obj::Pearl,
     Obj::Spices, Obj::Batteries, Obj::Axe,
     Obj::Oil, Obj::Water, Obj::Bottle, Obj::Food,
     Obj::Knife, Obj::Mag, Obj::Oyster, Obj::Clam,
     Obj::Pillow, Obj::Bird, Obj::Rod2, Obj::Rod,
     Obj::Cage, Obj::Lamp, Obj::Keys];

    let mut a = [false; N_OBJECTS];
    let mut i = 0;
    while i < l.len() {
        a[l[i] as usize] = true;
        i += 1;
    }
    a
}

fn is_treasure(obj: Obj) -> bool {
    obj >= Obj::Gold
}

const fn init_obj_props() -> [i8; N_OBJECTS] {
    let mut o2p = [0i8; N_OBJECTS];
    let mut i = Obj::Gold as usize;
    while i <= Obj::Chain as usize {
        o2p[i] = -1;
        i += 1;
    }
    o2p
}

//const fn init_obj_props() -> [i8; OBJECTS.len()] {
//    let mut o2p = [0i8; OBJECTS.len()];
//    let mut i = 0;
//    while i < OBJECTS.len() {
//        o2p[i] = if is_treasure(&OBJECTS[i]) { -1 } else { 0 };
//        i += 1;
//    }
//    o2p
//}

fn init_loc2obj_map() -> [Vec<Obj>; N_LOC] {
    const V: Vec<Obj> = Vec::<Obj>::new();
    let mut a = [V; N_LOC];
    for tup in [
        (
            Loc::Limbo,
            vec![
                Obj::Troll2,
                Obj::Mirror,
                Obj::Pearl,
                Obj::Chest,
                Obj::Batteries,
                Obj::Message,
                Obj::Pirate,
                Obj::Axe,
                Obj::Oil,
                Obj::Water,
                Obj::Knife,
                Obj::Dwarf,
                Obj::Oyster,
                Obj::Rod2,
            ],
        ),
        (Loc::Scan3, vec![Obj::Rug, Obj::Dragon]),
        (Loc::Scan1, vec![Obj::Rug, Obj::Dragon]),
        (Loc::Neside, vec![Obj::Troll, Obj::Bridge]),
        (Loc::Swside, vec![Obj::Troll, Obj::Bridge]),
        (Loc::Window, vec![Obj::Shadow]),
        (Loc::Windoe, vec![Obj::Shadow]),
        (Loc::E2pit, vec![Obj::Plant2]),
        (Loc::W2pit, vec![Obj::Plant2]),
        (Loc::Efiss, vec![Obj::Crystal]),
        (Loc::Spit, vec![Obj::Treads]),
        (Loc::Emist, vec![Obj::Treads]),
        (Loc::Inside, vec![Obj::Grate]),
        (Loc::Outside, vec![Obj::Grate]),
        (Loc::Barr, vec![Obj::Chain, Obj::Bear]),
        (Loc::Chamber, vec![Obj::Spices]),
        (Loc::Droom, vec![Obj::Pyramid, Obj::Tablet]),
        (Loc::Proom, vec![Obj::Emerald]),
        (Loc::Oriental, vec![Obj::Vase, Obj::Art]),
        (Loc::Falls, vec![Obj::Trident]),
        (Loc::Giant, vec![Obj::Eggs]),
        (Loc::West, vec![Obj::Coins]),
        (Loc::South, vec![Obj::Jewels]),
        (Loc::Ns, vec![Obj::Silver]),
        (Loc::Wfiss, vec![Obj::Diamonds]),
        (Loc::Nugget, vec![Obj::Gold]),
        (Loc::Soft, vec![Obj::Moss, Obj::Pillow]),
        (Loc::Pony, vec![Obj::Pony]),
        (Loc::View, vec![Obj::Geyser]),
        (Loc::Tite, vec![Obj::Stalactite]),
        (Loc::Wpit, vec![Obj::Plant]),
        (Loc::Mirror, vec![Obj::Mirror]),
        (Loc::Ante, vec![Obj::Mag]),
        (Loc::Shell, vec![Obj::Clam]),
        (Loc::Hmk, vec![Obj::Snake]),
        (Loc::Immense, vec![Obj::Door]),
        (Loc::Bird, vec![Obj::Bird]),
        (Loc::Debris, vec![Obj::Rod]),
        (Loc::Cobbles, vec![Obj::Cage]),
        (
            Loc::House,
            vec![Obj::Bottle, Obj::Food, Obj::Lamp, Obj::Keys],
        ),
    ] {
        a[tup.0] = tup.1;
    }
    a
}

const ND: usize = 5; // number of dwarves
const MAX_SCORE: u16 = 350;

struct Game {
    is_movable: [bool; N_OBJECTS],
    gave_up: bool,
    bonus: i32,
    k: i32,
    dtotal: u32,     // this many dwarves are in the room with you
    attack: u32,     // this many have had time to draw their knives
    stick: usize,    // this many have hurled their knives accurately
    ploc: [Loc; 19], // potential locations for the next random step
    foobar: i32,
    warned: bool, // have you been warned about the low power supply?
    death_count: usize,
    interval: u16,
    limit: i32,
    clock1: i8,
    clock2: i8,
    panic: bool,
    closed: bool,
    look_count: u16,
    was_dark: bool,
    knife_loc: Loc,  // most recent dwarf knife attack
    west_count: u16, // how many times have we parsed the word ‘west’?
    w: Word,
    loc: Loc,
    newloc: Loc,
    oldloc: Loc,
    oldoldloc: Loc,
    mot: Mot,
    verb: Act,
    oldverb: Act,
    obj: Obj,
    oldobj: Obj,
    turns: u32,
    tally: u8,
    lost_treasures: u8,
    hint_count: [u16; N_HINTS],
    hinted: [bool; N_HINTS],
    words: Vec<String>,
    visits: [u16; Loc::Nowhere as usize + 1],
    prop: [i8; N_OBJECTS],
    l2o: [Vec<Obj>; N_LOC],

    dflag: u8,             // how angry are the dwarves?
    dkill: u8,             // how many of them have you killed?
    dloc: [Loc; 1 + ND],   //
    odloc: [Loc; 1 + ND],  //prior locations
    dseen: [bool; 1 + ND], // have you been spotted
}

impl Game {
    fn new() -> Self {
        Game {
            is_movable: init_movable(),
            gave_up: false,
            bonus: 0,
            k: 0,
            dtotal: 0,
            attack: 0,
            stick: 0,
            ploc: [Loc::Nowhere; 19],
            foobar: 0,
            warned: false,
            death_count: 0,
            interval: 5u16,
            limit: 0,
            look_count: 0,
            clock1: 15,
            clock2: 30,
            panic: false,
            closed: false,
            was_dark: false,
            knife_loc: Loc::Nowhere,
            west_count: 0,
            w: Word::Action(Act::Abstain),
            loc: Loc::Road,
            newloc: Loc::Road,
            oldloc: Loc::Road,
            oldoldloc: Loc::Road,
            mot: Mot::Nowhere,
            verb: Act::Abstain,
            oldverb: Act::Abstain,
            obj: Obj::Nothing,
            oldobj: Obj::Nothing,
            turns: 0,
            tally: (Obj::Chain as usize - Obj::Gold as usize + 1) as u8,
            lost_treasures: 0,
            hint_count: [0, 0, 4, 5, 8, 75, 25, 20],
            hinted: [false; N_HINTS],
            words: Vec::new(),
            visits: [0; Loc::Nowhere as usize + 1],
            prop: init_obj_props(),
            l2o: init_loc2obj_map(),

            dflag: 0, // how angry are the dwarves?
            dkill: 0, // how many of them have you killed?
            dloc: [
                Loc::Dead2,
                Loc::Hmk,
                Loc::Wfiss,
                Loc::Y2,
                Loc::Like3,
                Loc::Complex,
            ],
            odloc: [
                Loc::Dead2,
                Loc::Hmk,
                Loc::Wfiss,
                Loc::Y2,
                Loc::Like3,
                Loc::Complex,
            ],
            dseen: [false; 6], // have you been spotted
        }
    }

    fn visits_inc(&mut self) {
        self.visits[self.loc as usize] += 1;
    }

    fn visits_zero(&mut self) {
        self.visits[self.loc as usize] = 0;
    }

    pub fn visits(&mut self) -> u16 {
        self.visits[self.loc as usize]
    }

    fn carry(&mut self, obj: Obj) {
        if !self.is_movable[obj as usize] {
            panic!("not movable");
        }
        self.remove(obj);
        self.l2o[Loc::Inhand].push(obj);
    }

    fn holding(&self) -> usize {
        let mut n=self.l2o[Loc::Inhand].len();
        //TODO - bird+cage, bottle+water/oil
        if self.toting(Obj::Cage) && self.prop[Obj::Bird]==1 {
            n+=1;
        }
        if self.toting(Obj::Bottle) && self.prop[Obj::Bird]!=1 {
            n+=1;
        }
        n
    }

    pub fn smash_vase(&mut self) {
        self.prop[Obj::Vase] = 2;
        self.is_movable[Obj::Vase] = false;
    }

    pub fn bottle_empty(&self) -> bool {
        self.prop[Obj::Bottle] == 1 || self.prop[Obj::Bottle] < 0
    }

    // remove object from all locations
    fn remove(&mut self, obj: Obj) {
        for j in 0..self.l2o.len() {
            if let Some(i) = self.l2o[j].iter().position(|v| *v == obj) {
                self.l2o[j].swap_remove(i);
            }
        }
    }

    // place obj at loc (unique).
    fn drop(&mut self, obj: Obj, loc: Loc) {
        self.remove(obj); // remove unique object
        self.l2o[loc].push(obj);
    }

    // put obj at loc
    fn put(&mut self, obj: Obj, loc: Loc) {
        self.l2o[loc].push(obj);
    }

    //#define here(t) (toting(t) ∨ place[t] ≡ loc) /∗ is object t present? ∗/
    fn toting(&self, obj: Obj) -> bool {
        let (flag, obj) = match obj {
            Obj::Water => (self.prop[Obj::Bottle] == 0, Obj::Bottle),
            Obj::Oil => (self.prop[Obj::Bottle] == 2, Obj::Bottle),
            _ => (true, obj),
        };
        flag && self.l2o[Loc::Inhand].iter().any(|&o| o == obj)
    }

    fn is_here(&self, obj: Obj) -> bool {
        self.l2o[self.loc].iter().any(|&o| o == obj)
    }

    fn here(&self, obj: Obj) -> bool {
        if self.toting(obj) {
            return true;
        }

        self.is_here(obj)
    }

    fn is_at(&self, obj: Obj, loc: Loc) -> bool {
        self.l2o[loc].iter().any(|&o| o == obj)
    }

    // return location of obj
    fn place(&self, obj: Obj) -> Vec<Loc> {
        let mut v = Vec::new();
        for l in LOCATIONS {
            if self.is_at(obj, l) {
                v.push(l)
            }
        }
        v
    }

    fn closing(&self) -> bool {
        self.clock1 < 0
    }

    fn dark(&self) -> bool {
        condition(self.loc) & LIGHTED == 0 && (self.prop[Obj::Lamp] == 0 || !self.here(Obj::Lamp))
    }

    fn object_in_bottle(&self, obj: Obj) -> bool {
        (obj == Obj::Water && self.prop[Obj::Bottle] == 0)
            || (obj == Obj::Oil && self.prop[Obj::Bottle] == 2)
    }

    // 194
    fn offer(&mut self, j: usize) {
        if j > 1 {
            if !yes(HINT[j].0, "I am prepared to give you a hint", OK) {
                return;
            }
            println!(" but it will cost you {} points", HINT_COST[j]);
            self.hinted[j] = yes("Do you want the hint?", HINT[j].1, OK);
        } else {
            self.hinted[j] = yes(HINT[j].0, HINT[j].1, OK);
        }
        if self.hinted[j] && self.limit > 30 {
            self.limit += 30 * HINT_COST[j]
        }
    }

    // is a dwarf present
    fn dwarf(&self) -> bool {
        if self.dflag < 2 {
            return false;
        }
        self.dloc.iter().skip(1).any(|&l| l == self.loc) // skip pirate
    }
}

enum Goto {
    DwarvesUpset,
    Branch,
    GetObject,
    GiveUp,
    Cycle,
    Death,
    PitchDark,
    TryMove,
    Quit,
    Commence,
    Major,
    Minor,
    Parse,
    PreParse,
    Transitive,
    Intransitive,
    GoForIt,
    CantSeeIt,
}

fn quit(g: &Game) -> Goto {
    //⟨ Print the score and say adieu 198 ⟩=;
    static CLASS_SCORE: [i32; 9] = [35, 100, 130, 200, 250, 300, 330, 349, std::i32::MAX];

    let k = score(g);

    let mut j = 0;
    while CLASS_SCORE[j] <= k {
        j += 1;
    }

    println!(
        "You scored {k} points out of a possible {MAX_SCORE}, using {} turn{}.\n",
        g.turns,
        if g.turns == 1 { "" } else { "s" }
    );
    let s = match j {
        0 => "You are obviously a rank amateur. Better luck next time.",
        1 => "Your score qualifies you as a novice class adventurer.",
        2 => "You have achieved the rating \"Experienced␣Adventurer\".",
        3 => "You may now consider␣yourself a \"Seasoned␣Adventurer\".",
        4 => "You have reached \"Junior␣Master\" status.",
        5 => "Your score puts you in Master Adventurer Class C.",
        6 => "Your score puts you in Master Adventurer Class B.",
        7 => "Your score puts you in Master Adventurer Class A.",
        _ => "All of Adventuredom gives tribute to you, Adventure Grandmaster!",
    };
    print!("{s}\nTo achieve the next higher rating");
    if j < CLASS_SCORE.len() - 1 {
        println!(
            ", you need {} more point{}.",
            CLASS_SCORE[j] - k,
            if CLASS_SCORE[j] == k + 1 { "" } else { "s" }
        );
    } else {
        print!(" would be a neat trick!\nCongratulations!!\n");
    }
    process::exit(1);
}

fn death(g: &mut Game) -> Goto {
    //⟨ Deal with death and resurrection 188 ⟩=;
    g.death_count += 1;
    if g.closing() {
        println!("It looks as though you're dead. Well, seeing as how it's so close to closing time anyway, let's just call it a day.");
        return Goto::Quit;
    }
    if !yes(
        DEATH_WISHES[2 * g.death_count - 2],
        DEATH_WISHES[2 * g.death_count - 1],
        OK,
    ) || g.death_count == MAX_DEATHS
    {
        return Goto::Quit;
    }

    if g.toting(Obj::Lamp) {
        g.prop[Obj::Lamp] = 0;
        g.drop(Obj::Lamp, Loc::Road);
    }
    for o in g.l2o[Loc::Inhand].clone() {
        g.drop(o, g.oldoldloc);
    }

    g.remove(Obj::Water);
    g.remove(Obj::Oil);
    g.loc = Loc::House;
    g.oldloc = Loc::House;
    return Goto::Commence;
}

fn pitch_dark(g: &mut Game) -> Goto {
    println!("You fell into a pit and broke every bone in your body!");
    g.oldoldloc = g.loc;
    return Goto::Quit;
}

fn dwarves_upset() -> Goto {
    println!("The resulting ruckus has awakened the dwarves. There are now several threatening little dwarves in the room with you! Most of them throw knives at you! All of them get you!");
    Goto::PitchDark
}

fn panic_at_closing_time(g: &mut Game) -> &str {
    //⟨ Panic at closing time 180 ⟩ ≡
    if !g.panic {
        g.clock2 = 15;
        g.panic = true;
    }
    "A mysterious recorded voice groans into life and announces: \"This exit is closed. Please leave via main office.\""
}

fn pirate_not_spotted(g: &Game) -> bool {
    //#define pirate not spotted (place[MESSAGE] ≡ limbo)
    //TODO limbo vs empty
    g.place(Obj::Message).is_empty() || g.is_at(Obj::Message, Loc::Limbo)
}

fn too_easy(g: &Game, o: Obj) -> bool {
    //#define too easy (i) (i ≡ PYRAMID ∧ (loc ≡ proom ∨ loc ≡ droom ))
    o == Obj::Pyramid && matches!(g.loc, Loc::Proom | Loc::Droom)
}

fn make_pirate_track_you(g: &mut Game) {
    //⟨ Make the pirate track you 172 ⟩ ≡
    if g.loc != MAX_PIRATE_LOC && g.prop[Obj::Chest] < 0 {
        for o in TREASURES {
            if !too_easy(g, o) & g.toting(o) {
                g.k = -1;
                break;
            }
            if g.here(o) {
                g.k = 1;
            }
        }
        let s = if g.k < 0 {
            //⟨ Take booty and hide it in the chest 173 ⟩ ≡
            //⟨ Snatch all treasures that are snatchable here 174 ⟩ ≡
            for o in TREASURES {
                if !too_easy(g, o) {
                    if g.is_movable[o] && g.is_here(o) {
                        g.drop(o, CHEST_LOC)
                    }
                }

                if pirate_not_spotted(g) {
                    // move chest
                    g.drop(Obj::Chest, CHEST_LOC);
                    g.drop(Obj::Message, MESSAGE_LOC);
                }
                g.dloc[0] = CHEST_LOC;
                g.odloc[0] = CHEST_LOC;
                g.dseen[0] = false;
            }
            "Out from the shadows behind you pounces a bearded pirate! \"Har, har,\" he␣chortles, \"I’ll just take all this booty and hide it away with me chest deep in the maze!\" He snatches your treasure and vanishes into the gloom."
        } else if g.tally == g.lost_treasures + 1
            && g.k == 0
            && pirate_not_spotted(g)
            && g.prop[Obj::Lamp] != 00
            && g.here(Obj::Lamp)
        {
            //⟨ Let the pirate be spotted 175 ⟩ ≡
            // move chest
            g.drop(Obj::Chest, CHEST_LOC);
            g.drop(Obj::Message, MESSAGE_LOC);
            g.dloc[0] = CHEST_LOC;
            g.odloc[0] = CHEST_LOC;
            g.dseen[0] = false;
            "There are faint rustling noises from the darkness behind you. As you turn toward them, the beam of your lamp falls across a bearded pirate. He is carrying a large chest. \"Shiver me timbers!\" he cries, \"I’ve been spotted! I’d best hie meself off to the maze to hide me chest!\" With that, he vanishes into the gloom."
        } else if g.odloc[0] != g.dloc[0] && pct(20) {
            "There are faint rustling noises from the darkness behind you."
        } else {
            ""
        };
        if s != "" {
            println!("{s}")
        };
    }
}

fn major(g: &mut Game) -> Goto {
    //⟨ Check for interference with the proposed move to newloc 153 ⟩ ≡
    if g.closing() && g.newloc < MIN_IN_CAVE && g.newloc != Loc::Limbo {
        g.newloc = g.loc;
        println!("{}", panic_at_closing_time(g));
    } else if g.newloc != g.loc {
        //⟨ Stay in loc if a dwarf is blocking the way to newloc 176 ⟩ ≡
        if g.newloc <= MAX_PIRATE_LOC {
            if (1..g.odloc.len()).any(|j| g.odloc[j] == g.newloc && g.dseen[j]) {
                println!("A little dwarf with a big knife blocks your way.");
                g.newloc = g.loc;
            }
        }
    }
    g.loc = g.newloc; // hey, we actually moved you

    //⟨ Possibly move dwarves and the pirate 161 ⟩ ≡
    if g.loc <= MAX_PIRATE_LOC && g.loc != Loc::Limbo {
        match g.dflag {
            0 if g.loc >= MIN_LOWER_LOC => g.dflag = 1,
            1 if g.loc >= MIN_LOWER_LOC && pct(5) => {
                //⟨ Advance dflag to 2 162 ⟩ ≡
                g.dflag = 2;
                for j in 0..2 {
                    if pct(50) {
                        g.dloc[1 + ran(ND)] = Loc::Limbo;
                    }
                }

                for j in 1..=ND {
                    if g.dloc[j] == g.loc {
                        g.dloc[j] = Loc::Nugget;
                    }
                    g.odloc[j] = g.dloc[j];
                }
                println!("A little dwarf just walked around a corner, saw you, threw a little axe at you, cursed, and ran away. (The axe missed.)");
                g.drop(Obj::Axe, g.loc);
            }
            _ => {
                //⟨ Move dwarves and the pirate 164 ⟩ ≡
                g.dtotal = 0; // initialize totals for possible battles
                g.attack = 0;
                g.stick = 0;
                for j in 0..g.dloc.len() {
                    if g.dloc[j] != Loc::Limbo {
                        let mut i = 0;
                        //⟨ Make a table of all potential exits, ploc [0] through ploc [i − 1] 166 ⟩;
                        if i == 0 {
                            i = 1;
                            g.ploc[0] = g.odloc[j];
                        }
                        g.odloc[j] = g.dloc[j];
                        g.dloc[j] = g.ploc[ran(i)]; // random walk
                        g.dseen[j] = g.dloc[j] == g.loc
                            || g.odloc[j] == g.loc
                            || (g.dseen[j] && g.loc >= MIN_LOWER_LOC);
                        if g.dseen[j] {
                            //⟨ Make dwarf j follow 167 ⟩ ≡
                            g.dloc[j] = g.loc;
                            if j == 0 {
                                make_pirate_track_you(g)
                            } else {
                                g.dtotal += 1;
                                if g.odloc[j] == g.dloc[j] {
                                    g.attack += 1;
                                    if !matches!(g.knife_loc, Loc::Inhand | Loc::Limbo) {
                                        g.knife_loc = g.loc;
                                    }
                                    if (ran(1000) as i32) < 95 * (g.dflag as i32 - 2) {
                                        g.stick += 1;
                                    }
                                }
                            }
                        }
                    }
                } // for each dwarf+pirate
                if g.dtotal > 0 {
                    //⟨ Make the threatening dwarves attack 170 ⟩ ≡
                    if g.dtotal == 1 {
                        print!("There is a threatening little dwarf");
                    } else {
                        print!("There are {} threatening little dwarves", g.dtotal);
                    }
                    println!(" in the room with you!");
                    if g.attack > 0 {
                        if g.dflag == 2 {
                            g.dflag = 3;
                        }
                        if g.attack == 1 {
                            print!("One sharp nasty knife is thrown");
                        } else {
                            print!("{} of them throw knives", g.attack);
                        };
                        print!(" at you --- ");
                        match (g.attack, g.stick) {
                            (1, 0) => println!("it misses!"),
                            (1, 1) => println!("it gets you!"),
                            (_, 0) => println!("none of them hit you"),
                            (_, 1) => println!("one of them gets you"),
                            (_, _) => println!("{} of them get you!", g.stick),
                        };
                        if g.stick > 0 {
                            g.oldoldloc = g.loc;
                            return Goto::Death;
                        }
                    }
                } // if dtotal
            } // possibly move dwarves+pirate
        }
    }
    Goto::Commence
}

fn minor(g: &mut Game) -> Goto {
    //⟨ Get user input; goto try move if motion is requested 76 ⟩ ≡
    g.verb = Act::Abstain;
    g.oldverb = Act::Abstain;
    g.oldobj = g.obj;
    g.obj = Obj::Nothing;
    return Goto::Cycle;
}

fn cycle(g: &mut Game) -> Goto {
    //⟨ Check if a hint applies, and give it if requested 195 ⟩ ≡
    let bc = g.here(Obj::Bird) && g.oldobj == Obj::Bird && g.toting(Obj::Rod);

    for j in 2..N_HINTS {
        if !g.hinted[j] {
            if (condition(g.loc) & 2u16.pow(1 + j as u32)) == 0 {
                g.hint_count[j] = 0;
            } else if g.hint_count[j] >= HINT_THRESH[j] {
                if match j {
                    2 => g.prop[Obj::Grate] == 0 && !g.here(Obj::Keys),
                    3 if !bc => continue,
                    4 => g.here(Obj::Snake) && !g.here(Obj::Bird),
                    5 => {
                        g.l2o[g.loc].len() == 0
                            && g.l2o[g.oldloc].len() == 0
                            && g.l2o[g.oldoldloc].len() == 0
                            && g.l2o[Loc::Inhand].len() > 1
                    }
                    6 => g.prop[Obj::Emerald] != -1 && g.prop[Obj::Pyramid] == -1,
                    _ => true,
                } {
                    g.offer(j)
                }
                g.hint_count[j] = 0;
            }
        }
    }

    //⟨ Make special adjustments before looking at new input 85 ⟩ ≡
    //See also sections 158, 169, and 182.
    g.was_dark = g.dark();
    g.k = 0;
    if g.knife_loc > Loc::Limbo && g.knife_loc != g.loc {
        g.knife_loc = Loc::Limbo;
    }

    if g.closed {
        if g.prop[Obj::Oyster] < 0 && g.toting(Obj::Oyster) {
            let note = object_notes(Obj::Oyster, g.prop[Obj::Oyster]);
            println!("{note}");
        }
        for o in g.l2o[Loc::Inhand].iter() {
            if g.prop[*o] < 0 {
                g.prop[*o] = -1 - g.prop[*o];
            }
        }
    }

    g.words = listen();

    Goto::PreParse
}

fn pre_parse(g: &mut Game) -> Goto {
    g.turns += 1;
    //⟨ Handle special cases of input 82 ⟩ ≡
    if g.verb == Act::Say {
        if g.words.len() > 1 {
            g.verb = Act::Abstain;
        } else {
            return Goto::Transitive;
        }
    }

    g.foobar = if g.foobar > 0 { -g.foobar } else { 0 };

    //⟨ Check the clocks and the lamp 178 ⟩ ≡
    if g.tally == 0 && g.loc >= MIN_LOWER_LOC && g.loc != Loc::Y2 {
        g.clock1 -= 1;
    }
    if g.clock1 == 0 {
        //⟨ Warn that the cave is closing 179 ⟩ ≡
        println!("A sepulchral voice, reverberating through the cave, says, \"Cave closing soon. All adventurers exit immediately through main office.\"");
        g.clock1 = -1;
        g.prop[Obj::Grate] = 0;
        g.prop[Obj::Crystal] = 0;
        for j in 0..g.dseen.len() {
            g.dseen[j] = false;
            g.dloc[j] = Loc::Limbo;
        }
        g.remove(Obj::Troll);
        g.remove(Obj::Troll2);
        g.l2o[Loc::Swside].push(Obj::Troll2);
        g.l2o[Loc::Neside].push(Obj::Troll2);
        g.remove(Obj::Bridge);
        g.l2o[Loc::Swside].push(Obj::Bridge);
        g.l2o[Loc::Neside].push(Obj::Bridge);

        if g.prop[Obj::Bear] != 3 {
            g.remove(Obj::Bear);
        }
        for o in [Obj::Chain, Obj::Axe] {
            g.prop[o] = 0;
            g.is_movable[o] = true;
        }
    } else {
        if g.clock1 < 0 {
            g.clock2 -= 1
        }
        if g.clock2 == 0 {
            //⟨ Close the cave 181 ⟩ ≡
            println!("The sepulchral voice intones, \"The cave is now closed.\" As the echoes fade, there is a blinding flash of light (and a small puff of orange smoke). . . . Then your eyes refocus; you look around and find...");
            for (obj, p) in [
                (Obj::Bottle, -2),
                (Obj::Plant, -1),
                (Obj::Oyster, -1),
                (Obj::Lamp, -1),
                (Obj::Rod, -1),
                (Obj::Dwarf, -1),
                (Obj::Mirror, -1),
            ] {
                g.remove(obj);
                g.l2o[Loc::Neend].push(obj);
                g.prop[obj] = p
            }
            g.loc = Loc::Neend;
            g.oldloc = Loc::Neend;

            for (obj, p) in [
                (Obj::Grate, 0),
                (Obj::Snake, -2),
                (Obj::Bird, -2),
                (Obj::Cage, -1),
                (Obj::Rod2, -1),
                (Obj::Pillow, -1),
            ] {
                g.remove(obj);
                g.l2o[Loc::Swend].push(obj);
                g.prop[obj] = p
            }
            g.l2o[Loc::Swend].push(Obj::Mirror);
            for obj in g.l2o[Loc::Inhand].clone().iter() {
                g.remove(*obj)
            }
            g.closed = true;
            g.bonus = 10;
            g.mot = Mot::Nowhere;
            return Goto::TryMove;
        } else {
            //⟨ Check the lamp 184 ⟩ ≡
            if g.prop[Obj::Lamp] == 1 {
                g.limit -= 1;
            }

            if g.limit <= 30
                && g.here(Obj::Batteries)
                && g.prop[Obj::Batteries] == 0
                && g.here(Obj::Lamp)
            {
                //⟨ Replace the batteries 186 ⟩ ≡
                println!(
                    "Your lamp is getting dim. I'm taking the liberty of replacing the batteries."
                );
                g.prop[Obj::Batteries] = 1;
                if g.toting(Obj::Batteries) {
                    g.drop(Obj::Batteries, g.loc);
                }
                g.limit = 2500;
            } else if g.limit == 0 {
                //⟨ Extinguish the lamp 187 ⟩ ≡
                g.limit = -1;
                g.prop[Obj::Lamp] = 0;
                if g.here(Obj::Lamp) {
                    println!("Your lamp has run out of power.");
                }
            } else if g.limit < 0 && g.loc < MIN_IN_CAVE {
                println!("There's not much point in wandering around out here, and you can't explore the cave without a lamp. So let's just call it a day.");
                return Goto::GiveUp;
            } else if g.limit <= 30 && !g.warned && g.here(Obj::Lamp) {
                let s = if g.prop[Obj::Batteries] == 1 {
                    ", and you're out of spare batteries. You'd best start wrapping this up."
                } else if g.is_at(Obj::Batteries, Loc::Limbo) {
                    //TODO - Limbo or removed?
                    ". You'd best start wrapping this up, unless you can find some fresh batteries. I seem to recall that there's a vending machine in the maze. Bring some coins with you."
                } else {
                    ". You'd best go back for those batteries."
                };
                println!("Your lamp is getting dim{}", s);
                g.warned = true;
            }
        }
    }

    //⟨ Handle additional special cases of input 83 ⟩ ≡
    if g.words[0] == "enter" {
        if g.words.len() > 1 && matches!(g.words[1].as_str(), "water" | "strea") {
            if water_here(g.loc) {
                println!("Your feet are now wet.");
            }
            println!("{}", default_msg(Act::Go));
            return Goto::Minor;
        } else if g.words.len() > 1 {
            g.words.remove(0);
            return Goto::Parse;
        }
    }

    // water/oil used as verbs
    if g.words.len() > 1 {
        if let Word::Object(o) = lookup_word(g.words[1].as_str()) {
            if (g.words[0] == "water" || g.words[0] == "oil")
                && matches!(o, Obj::Plant | Obj::Door)
                && g.is_at(o, g.loc)
            {
                g.words[1] = String::from("pour");
            }
        }
    }

    Goto::Parse
}

fn parse(g: &mut Game) -> Goto {
    //⟨ Give advice about going WEST 80 ⟩ ≡
    if g.words[0] == "west" {
        g.west_count += 1;
        if g.west_count == 10 {
            println!(" If you prefer, simply type W rather than WEST.");
        }
    }

    //#define try motion (m) { mot = m; goto try move ; }
    //#define stay put try motion (NOWHERE)
    //⟨ Look at word1 and exit to the right place if it completes a command 78 ⟩ ≡
    g.w = lookup_word(g.words[0].as_str());

    Goto::Branch
}

fn branch(g: &mut Game) -> Goto {
    match &g.w {
        Word::None => {
            println!("Sorry, I don't know the word \"{}\".", g.words[0]);
            return Goto::Cycle;
        }
        Word::Motion(m) => {
            g.mot = *m;
            return Goto::TryMove;
        }
        Word::Object(o) => {
            g.obj = *o;
            //⟨ Make sure obj is meaningful at the current location 90 ⟩ ≡
            if !g.toting(*o) && !g.is_here(*o) {
                match o {
                    //⟨ If GRATE is actually a motion word, move to it 91 ⟩ ≡
                    Obj::Grate if matches!(g.loc, Loc::Road | Loc::Valley | Loc::Slit) => {
                        g.mot = Mot::Depression;
                        return Goto::TryMove;
                    }
                    Obj::Grate
                        if matches!(
                            g.loc,
                            Loc::Cobbles | Loc::Debris | Loc::Awk | Loc::Bird | Loc::Spit
                        ) =>
                    {
                        g.mot = Mot::Entrance;
                        return Goto::TryMove;
                    }
                    Obj::Dwarf if g.dflag >= 2 && g.dwarf() => (),
                    Obj::Plant if g.is_here(Obj::Plant2) && g.prop[Obj::Plant2] != 0 => {
                        g.obj = Obj::Plant2
                    }

                    Obj::Knife if g.loc == g.knife_loc => {
                        g.knife_loc = Loc::Limbo;
                        println!(
                            "The dwarves' knives vanish as they strike the walls of the cave."
                        );
                    }
                    Obj::Rod if g.here(Obj::Rod2) => g.obj = Obj::Rod2,
                    Obj::Water
                        if water_here(g.loc) || (g.here(Obj::Bottle) && g.object_in_bottle(*o)) =>
                    {
                        ()
                    }
                    Obj::Oil
                        if oil_here(g.loc) || (g.here(Obj::Bottle) && g.object_in_bottle(*o)) =>
                    {
                        ()
                    }

                    _ => return Goto::CantSeeIt,
                }
            }

            if g.words.len() == 1 {
                if g.verb != Act::Abstain {
                    return Goto::Transitive;
                };
                println!("What do you want to do with the {}?", g.words[0]);
                return Goto::Cycle;
            }
        }

        Word::Action(Act::Say) => {
            g.verb = Act::Say;
            if g.words.len() > 1 {
                return Goto::Transitive;
            } else {
                return Goto::Intransitive;
            }
        }
        Word::Action(a) => {
            g.verb = *a;
            if g.words.len() == 1 {
                if g.obj == Obj::Nothing {
                    return Goto::Intransitive;
                } else {
                    return Goto::Transitive;
                }
            }
        }

        Word::Message(m) => {
            println!("{}", m);
            return Goto::Minor;
        }
    }

    g.words.remove(0);
    return Goto::Parse;
}

fn get_object(g: &Game) -> Goto {
    println!("{} what?", g.words[0]);
    Goto::Cycle
}

fn cant_see_it(g: &mut Game) -> Goto {
    if matches!(g.verb, Act::Find | Act::Inventory) && g.words[1] == "" {
        g.words.remove(0);
        return Goto::Transitive;
    }
    println!("I see no {} here.", g.words[0]);
    Goto::Minor
}

fn stay_put(g: &mut Game) -> Goto {
    g.mot = Mot::Nowhere;
    Goto::TryMove
}

fn change_to(g: &mut Game, verb: Act) -> Goto {
    g.oldverb = verb;
    g.verb = verb;
    Goto::Transitive
}

fn transitive(g: &mut Game) -> Goto {
    g.k = 0;
    let s=match g.verb {
        //⟨ Handle cases of transitive verbs and continue 97 ⟩ ≡
        Act::Say => {
            if g.words.len() > 1 {
                g.words.remove(0);
            }
            g.w = lookup_word(g.words[0].as_str());
            match g.w {
                Word::Motion(Mot::Xyzzy | Mot::Plugh | Mot::Plover) | Word::Action(Act::Feefie) => {
                    g.obj = Obj::Nothing;
                    return Goto::Branch;
                }
                _ => {
                    println!("Okay, \"{}\"", g.words[0]);
                    return Goto::Minor;
                }
            }
        }
        Act::Eat => {
            match g.obj {
                Obj::Food => "Thank you, it was delicious!",
                Obj::Bird
                | Obj::Snake
                | Obj::Clam
                | Obj::Oyster
                | Obj::Dwarf
                | Obj::Dragon
                | Obj::Troll
                | Obj::Bear => "I think I just lost my appetite.",
                _ => default_msg(Act::Eat),
            }
        }
        Act::Wave if g.obj==Obj::Rod && g.toting(Obj::Rod) && matches!(g.loc, Loc::Efiss | Loc::Wfiss) && !g.closing() => {
            g.prop[Obj::Crystal] = 1 - g.prop[Obj::Crystal];
            object_notes(Obj::Crystal, 2 - g.prop[Obj::Crystal])
        }
        Act::Wave if !g.toting(g.obj) =>
               default_msg(Act::Drop), // don't carry it 

        Act::Blast if g.closed && g.prop[Obj::Rod2] >= 0 => {
            println!("{}", if g.here(Obj::Rod2) {
                g.bonus=25;
                "There is a loud explosion and you are suddenly splashed across the walls of the room."
            } else if g.loc==Loc::Neend {
                g.bonus=30;
                "There is a loud explosion and a twenty-foot hole appears in the far wall, burying the snakes in the rubble. A river of molten lava pours in through the hole, destroying everything in its path, including you!"
            } else {
                g.bonus=45;
                "There is a loud explosion, and a twenty-foot hole appears in the far wall, burying the dwarves in the rubble. You march through the hole and find yourself in the main office, where a cheering band of friendly elves carry the conquering adventurer off into the sunset."
            });
            return Goto::Quit;
        }
        Act::Rub if g.obj != Obj::Lamp => default_msg(Act::Toss),
        Act::Find | Act::Inventory if g.toting(g.obj) => default_msg(Act::Take),
        Act::Find | Act::Inventory if g.closed => {
            "I daresay whatever you want is around here somewhere."
        }
        Act::Find | Act::Inventory
            if g.is_here(g.obj)
                || (g.object_in_bottle(g.obj) && g.is_here(Obj::Bottle))
                || (g.obj == Obj::Water && water_here(g.loc))
                || (g.obj == Obj::Oil && oil_here(g.loc))
                || (g.obj == Obj::Dwarf && g.dwarf()) =>
        {
            "I believe what you want is right here with you."
        }

        Act::Break if g.obj == Obj::Mirror => {
            if g.closed {
                println!("You strike the mirror a resounding blow, whereupon it shatters into a myriad tiny fragments.");
                return Goto::DwarvesUpset;
            } else {
                "It is too far up for you to reach"
            }
        }
        Act::Break if g.obj == Obj::Vase && g.prop[Obj::Vase] == 0 => {
            if g.toting(Obj::Vase) {
                g.drop(Obj::Vase, g.loc); // crash
            }
            g.smash_vase();
            "You have taken the vase and hurled it delicately to the ground."
        }

        Act::Wake if g.closed && g.obj == Obj::Dwarf => {
            println!("You prod the nearest dwarf, who wakes up grumpily, takes one look at you, curses, and grabs for his axe.");
            return Goto::DwarvesUpset;
        }

        Act::On if g.here(Obj::Lamp) => {
            if g.limit<0 {
                "Your lamp has run out of power."
            } else {
                g.prop[Obj::Lamp]=1;
                println!("Your lamp is now on.");
                if g.was_dark
                    {return Goto::Commence}
                ""
            }
        }

        Act::Off if g.here(Obj::Lamp) => {
            g.prop[Obj::Lamp]=0;
            println!("Your lamp is now off.");
            if g.dark() {PITCH_DARK_MSG} else {""}
        }

        Act::Drink if g.here(Obj::Bottle) && g.prop[Obj::Bottle] == 0 => {
            g.prop[Obj::Bottle] = 1;
            g.remove(Obj::Water);
            "The bottle of water is now empty."
        }
        Act::Drink
            if g.obj == Obj::Nothing
                && !water_here(g.loc)
                && !(g.here(Obj::Bottle) && g.prop[Obj::Bottle] == 0) =>
        {
            return Goto::GetObject;
        }
        Act::Drink if g.obj != Obj::Water => {
            default_msg(Act::Eat)
        }

        Act::Pour => {
            if matches!(g.obj, Obj::Nothing | Obj::Bottle) {
                g.obj=match g.prop[Obj::Bottle] {
                    0 => Obj::Water,
                    2 => Obj::Oil,
                    _ => Obj::Nothing,
                };
                if g.obj==Obj::Nothing {
                    return Goto::GetObject;
                }
            }
            if !g.toting(g.obj) {
                default_msg(g.verb)
            } else if !matches!(g.obj, Obj::Water | Obj::Oil) {
                "You can’t pour that." 
            } else {
                g.prop[Obj::Bottle] = 1;
                if g.is_here(Obj::Plant) {
                    //⟨ Try to water the plant 108 ⟩ ≡
                    if g.obj != Obj::Water {
                        "The plant indignantly shakes the oil off its leaves and asks, \"Water?\""
                    } else {
                        println!("{}", object_notes(Obj::Plant, g.prop[Obj::Plant]));
                        g.prop[Obj::Plant] += 2;
                        if g.prop[Obj::Plant] > 4 {
                            g.prop[Obj::Plant] = 0;
                        }
                        g.prop[Obj::Plant2] = g.prop[Obj::Plant] >> 1;
                        g.mot = Mot::Nowhere;
                        return Goto::TryMove;
                    }
                } else if g.is_here(Obj::Door) {
                    //⟨ Pour water or oil on the door 109 ⟩ ≡
                    if g.obj == Obj::Water {
                        g.prop[Obj::Door] = 0;
                        "The hinges are quite thoroughly rusted now and won't budge"
                    } else {
                        // Oil
                        g.prop[Obj::Door]=1;
                        "The oil has freed up the hinges so that the door will now open."
                    }
                } else {
                   "Your bottle is empty and␣the ground is wet."
                }
            }
        }

        Act::Fill if g.obj == Obj::Vase => {
            //⟨ Try to fill the vase 111 ⟩ ≡
            if no_liquid_here(g.loc) {
                "There is nothing here with which to fill the vase."
            } else if !g.toting(Obj::Vase) {
                default_msg(Act::Drop)
            } else {
                g.smash_vase();
                "The sudden change in temperature has delicately shattered the vase."
            }
        }

        Act::Fill if !g.here(Obj::Bottle) => return Goto::GetObject,
        Act::Fill if matches!(g.obj, Obj::Nothing | Obj::Bottle) => {
            if !g.bottle_empty() {
                "Your bottle is already full."
            } else if no_liquid_here(g.loc) {
                "There is nothing here with which to fill the bottle."
            } else {
                g.prop[Obj::Bottle]=(condition(g.loc) & OIL).try_into().unwrap();
                if g.prop[Obj::Bottle] == 2 {
                    "Your bottle is now full of oil."
                } else {
                    "Your bottle is now full of water."
                }
            }
        }
        Act::Take if !g.is_movable[g.obj] => {
            if g.obj==Obj::Chain && g.prop[Obj::Bear]!=0 {
                "The chain is still locked."
            } else if g.obj==Obj::Bear  && g.prop[Obj::Bear]==1 {
                "The bear is still chained to the wall."
            } else if g.obj==Obj::Plant  && g.prop[Obj::Plant]<=0 {
                "The plant has exceptionally deep roots and cannot be pulled free."
            } else {
                "You can't be serious!"
            }
        }
        Act::Take if g.holding()>=7 => {
            "You can't carry anything more. You'll hae to drop something first."
        }
        Act::Take if g.obj==Obj::Bird && !g.toting(Obj::Cage) => "You can catch the bird, but you cannot carry it.",
        Act::Take if g.obj==Obj::Bird && g.toting(Obj::Rod) => "The bird was unafraid when you entered, but as you approach it becomes disturbed and you cannot catch it.",

        Act::Take if matches!(g.obj, Obj::Water | Obj::Oil) && !g.toting(Obj::Bottle) => "You have nothing in which to carry it.",


        Act::Take if !g.toting(g.obj) => {
            if matches!(g.obj, Obj::Water | Obj::Oil) {
                g.obj=Obj::Bottle;
                if !g.object_in_bottle(g.obj) {
                    g.oldverb=g.verb;
                    g.verb=Act::Fill;
                    return Goto::Transitive;
                }
            }

            //TODO - bird in cage
            if g.obj==Obj::Bird && g.prop[Obj::Bird]==0 {
                g.prop[Obj::Bird]=1;
                g.obj=Obj::Cage;
                g.remove(Obj::Bird)
            }
            g.carry(g.obj);
            "OK."
        }

        Act::Drop if g.obj==Obj::Rod && !g.toting(Obj::Rod) && g.toting(Obj::Rod2) => {
            g.drop(Obj::Rod2, g.loc);
            "OK."
        }

        Act::Drop if !g.toting(g.obj) =>
             if matches!(g.obj, Obj::Water | Obj::Oil) && g.toting(Obj::Bottle) {
             g.drop(Obj::Bottle, g.loc);
             "OK."
        } else {
            default_msg(Act::Drop)
        }

        Act::Drop if g.obj==Obj::Coins && g.here(Obj::Pony) => {
            //⟨ Put coins in the vending machine 118 ⟩ ≡ 
            g.remove(Obj::Coins);
            g.drop(Obj::Batteries, g.loc);
            g.prop[Obj::Batteries]=0;
            object_notes(Obj::Batteries,0)
        }

        Act::Drop if g.obj==Obj::Bird =>
            //⟨ Check special cases for dropping the bird 120 ⟩ ≡ 
            if g.here(Obj::Snake) {
                println!("The little bird attacks the green snake, and in an astounding flurry drives the snake away."); 
                if g.closed {return Goto::DwarvesUpset};
                g.remove(Obj::Snake);
                g.prop[Obj::Snake]=1; // used in conditional instructions 
                ""
            } else if g.is_at(Obj::Dragon, g.loc) && g.prop[Obj::Dragon]==0 {
                g.remove(Obj::Bird);
                g.prop[Obj::Bird]=0;
                if g.is_at(Obj::Snake, Loc::Hmk) {
                    g.lost_treasures+=1;
                }
                "The little bird attacks the green dragon, and in an astounding flurry gets burnt to a cinder. The ashes blow away." 
            } else {
                g.prop[Obj::Bird]=1;
                g.drop(Obj::Bird, g.loc);
                "OK."
            }

        Act::Drop if g.obj==Obj::Vase && g.loc!=Loc::Soft => {
            //⟨ Check special cases for dropping the vase 121 ⟩ ≡ 
            if g.is_at(Obj::Pillow, g.loc) {
                g.prop[Obj::Vase]=0;
            } else {
                g.is_movable[Obj::Vase] = false;
                g.prop[Obj::Vase]=2;
            };
            object_notes(Obj::Vase, g.prop[Obj::Vase])
        }

        Act::Drop if g.obj==Obj::Bear && g.is_at(Obj::Troll, g.loc) => {
            //⟨ Chase the troll away 119 ⟩ ≡ 
            g.remove(Obj::Troll);
            g.drop(Obj::Troll2, Loc::Swside);
            g.drop(Obj::Troll2, Loc::Neside);
            g.prop[Obj::Troll]=2;
            "The bear lumbers toward the troll, who lets out a startled shriek and scurries away. The bear soon gives up the pursuit and wanders back."
        }

        Act::Drop => {
            g.drop(g.obj , g.loc );
            "OK."
        }

        Act::Toss if g.obj==Obj::Axe && g.dwarf() =>
        {
            //⟨Throw the axe at a dwarf 163⟩ ≡ 
            //TODO
            if let Some(j) = g.dloc.iter().take(1).position(|l| *l == g.loc) {

            let s=if ran(3) < 2 {
                g.dloc[j] = Loc::Limbo;
                g.dseen[j] = true;
                g.dkill+=1;
                if g.dkill ==1 {
                    "You killed a little dwarf. The body vanishes in a cloud of greasy black smoke."
                } else {"You␣killed␣a␣little␣dwarf."}
            } else {
             "You attack a little dwarf, but he dodges out of the way."
            };
               println!("{s}");
              g.drop(Obj::Axe,g.loc);
              stay_put(g);
        }
        ""
        }

        Act::Toss if g.obj==Obj::Rod && g.toting(Obj::Rod2) && !g.toting(Obj::Rod) => {
                g.obj=Obj::Rod2;
                change_to(g,Act::Drop);
                ""
            }

        Act::Toss if g.toting(g.obj) && is_treasure(g.obj) && g.is_at(Obj::Troll, g.loc) => {
            //⟨ Snarf a treasure for the troll 124 ⟩ ≡ 
            g.drop(g.obj, Loc::Limbo);
            g.remove(Obj::Troll);
            g.drop(Obj::Troll2,Loc::Swside);
            g.drop(Obj::Troll2,Loc::Neside);
            g.remove(Obj::Bridge);
            g.drop(Obj::Bridge, Loc::Swside);
            g.drop(Obj::Bridge, Loc::Neside);
            "The troll catches your treasure and scurries away out of sight."
        }

        Act::Toss if g.toting(g.obj) && g.obj==Obj::Food && g.here(Obj::Bear) => {
            g.obj=Obj::Bear;
            change_to(g,Act::Feed);
            ""
        }

        Act::Toss if g.toting(g.obj) && g.obj!=Obj::Axe => {
            change_to(g,Act::Drop);
            ""
        }

        Act::Toss if g.toting(g.obj) && g.obj==Obj::Axe => {
            let s=if g.is_at(Obj::Dragon, g.loc) && g.prop[Obj::Dragon] == 0 {
                "The axe bounces harmlessly off the dragon's thick scales." 
            } else if g.is_at(Obj::Troll,g.loc) {
                "The troll deftly catches the axe, examines it carefully, and tosses it back, declaring, \"Good workmanship, but it's not valuable enough.\""
            } else if g.here(Obj::Bear) && g.prop[Obj::Bear]==0 {
                //⟨Throw the axe at the bear 123⟩ ≡
                g.drop(Obj::Axe,g.loc);
                g.prop[Obj::Axe]=1;
                g.is_movable[Obj::Axe]=false;
                "The axe misses and lands near the bear where you can't get at it."
            } else {
                g.obj = Obj::Nothing;
                change_to(g,Act::Kill);
                ""
            };
            g.drop(Obj::Axe,g.loc);
            stay_put(g);
            s
         }

         Act::Kill  => {
            if g.obj==Obj::Nothing {
                 //⟨ See if there’s a unique object to attack 126 ⟩ ≡ {
                 if g.dwarf() {
                     g.k+=1;
                     g.obj=Obj::Dwarf;
                 }
                 if g.here(Obj::Snake) {
                     g.k+=1;
                     g.obj=Obj::Snake;
                 }
                 if g.is_at(Obj::Dragon,g.loc) && g.prop[Obj::Dragon]==0 {
                     g.k+=1;
                     g.obj=Obj::Dragon;
                 }
                 if g.is_at(Obj::Troll, g.loc) {
                     g.k+=1;
                     g.obj=Obj::Troll;
                 }
                 if g.here(Obj::Bear) && g.prop[Obj::Bear]==0 {
                     g.k+=1;
                     g.obj=Obj::Bear;
                 }
                 if g.k==0 { // no enemies present
                     if g.here(Obj::Bird) && g.oldverb!=Act::Toss {
                         g.k+=1;
                         g.obj=Obj::Bird;
                     }
                     // no harm done to call the oyster a clam in this case  
                     if g.here(Obj::Clam) || g.here(Obj::Oyster) {
                         g.k+=1;
                         g.obj=Obj::Clam;
                     }
                 }
            }
            if g.k > 1 {
                return Goto::GetObject;
            }

            match g.obj {
                Obj::Nothing => "There is nothing here to attack.",
                Obj::Bird => {
                    //⟨ Dispatch the poor bird 127 ⟩ ≡
                    if g.closed {
                        "Oh, leave the poor unhappy bird alone."
                    } else {
                        g.remove(Obj::Bird);
                        g.prop[Obj::Bird]=0;
                        if g.is_at(Obj::Snake,Loc::Hmk) {
                            g.lost_treasures+=1
                        }
                        "The little bird is now dead. Its body disappears."
                    }
                }
                Obj::Dragon if g.prop[Obj::Dragon]==0 =>{
                    //⟨ Fun stuff for dragon 128 ⟩ ≡ 
                    println!("With what? Your bare hands?");
                    g.verb = Act::Abstain;
                    g.obj = Obj::Nothing;
                    g.words=listen();
                    if g.words[0]!="yes" && g.words[0]!="y" {
                        return Goto::PreParse
                    }
                    g.prop[Obj::Dragon]=2; // dead
                    g.prop[Obj::Rug]=0;
                    g.is_movable[Obj::Rug] = true;
                    for l in [Loc::Scan1, Loc::Scan3] {
                        for o in g.l2o[l].clone() {
                            g.remove(o);
                            g.l2o[Loc::Scan2].push(o);
                        }
                    }
                    g.loc = Loc::Scan2;
                    stay_put(g);
                    object_notes(Obj::Dragon, 1)
                }
                Obj::Dragon if g.prop[Obj::Dragon]!=0 =>
                   "For crying out loud, the poor thing is already dead!",
                Obj::Clam | Obj::Oyster =>
                   "The shell is very strong and impervious to attack.",
                Obj::Snake =>
                   "Attacking the snake both doesn't work and is very dangerous.",
                Obj::Dwarf if g.closed => return Goto::DwarvesUpset,
                Obj::Dwarf => "With what? Your bare hands?",
                Obj::Troll => "Trolls are close relatives with the rocks and have skin as tough as a rhinoceros hide. The troll fends off your blows effortlessly.",
                Obj::Bear if g.prop[Obj::Bear]==0 =>
                    "With what? Your bare hands? Against HIS bear hands?",
                Obj::Bear if g.prop[Obj::Bear]==3 =>
                   "For crying out loud, the poor thing is already dead!",
                Obj::Bear =>
                   "The bear is confused; he only wants to be your friend.",
                _ => default_msg(Act::Kill),
            }
        }
        Act::Feed if g.obj==Obj::Bird  =>
            "It's not hungry (it's merely pinin' for the fjords). Besides, you have no bird seed.",
        Act::Feed if g.obj==Obj::Troll=>
              "Gluttony is not one of the troll's vices. Avarice, however, is.",
        Act::Feed if g.obj==Obj::Dragon && g.prop[Obj::Dragon]==0=>
         "There's nothing here it wants to eat (except perhaps you).",
        Act::Feed if g.obj==Obj::Dragon && g.prop[Obj::Dragon]!=0=>
         default_msg(Act::Eat),
        Act::Feed if g.obj==Obj::Snake && (g.closed || !g.here(Obj::Bird))=>
         "There's nothing here it wants to eat (except perhaps you).",
        Act::Feed if g.obj==Obj::Snake=> {
            g.remove(Obj::Bird);
            g.prop[Obj::Bird]=0;
            g.lost_treasures+=1;
            "The snake has now devoured your bird"
        }
        Act::Feed if g.obj==Obj::Bear && g.here(Obj::Food)=> {
            g.remove(Obj::Food);
            g.prop[Obj::Bear]=1;
            g.prop[Obj::Axe]=0;
            g.is_movable[Obj::Axe]=true;
        "The bear eagerly wolfs down your food, after which he seems to calm down considerably and even becomes rather friendly."
        }
        Act::Feed if g.obj==Obj::Bear && g.prop[Obj::Bear]==0 =>
         "There's nothing here it wants to eat (except perhaps you).",
        Act::Feed if g.obj==Obj::Bear && g.prop[Obj::Bear]==3 =>{
            default_msg(Act::Eat)
        }
        Act::Feed if g.obj==Obj::Dwarf && g.here(Obj::Food) => {
                g.dflag+=1;
                "You fool, dwarves eat only coal! Now you've made him REALLY mad!" 
        }
        Act::Feed => default_msg(Act::Calm),
        Act::Close if matches!(g.obj, Obj::Oyster|Obj::Clam) => "What?", 
        Act::Open if g.obj==Obj::Clam && !g.toting(Obj::Trident) =>
            "You don't have anything storng enough to open the clam.",
        Act::Open if g.obj==Obj::Oyster && !g.toting(Obj::Trident) =>
            "You don't have anything storng enough to open the oyster.",
        Act::Open if g.obj==Obj::Oyster && g.toting(g.obj) =>
            "I advise you to put down the oyster before opening it. >WRENCH!<",
        Act::Open if g.obj==Obj::Clam && g.toting(g.obj) =>
            "I advise you to put down the clam before opening it. >STRAIN!<",
        Act::Open if g.obj==Obj::Clam => {
            g.remove(Obj::Clam);
            g.drop(Obj::Oyster,g.loc);
            g.drop(Obj::Pearl,g.loc);
            "A glistening pearl falls out of the clam and rolls away. Goodness, this must really be an oyster. (I never was very good at identifying bivalves.) Whatever it is, it has now snapped shut again."
        }
        Act::Open if g.obj==Obj::Oyster =>
            "The oyster creaks open, revealing nothing but oyster inside. It promptly snaps shut again.",
        Act::Open | Act::Close if matches!(g.obj,Obj::Grate | Obj::Chain) && !g.here(Obj::Keys) => "You have no keys!",
        Act::Open if g.obj==Obj::Chain => {
            //⟨ Open chain 133 ⟩;
            if g.prop[Obj::Chain]==0 {
                "It was already unlocked."
            } else if g.prop[Obj::Bear]==0 {
                "There is no way to get past the bear to unlock the chain, which is probably just as well"
            } else  {
                g.prop[Obj::Chain]=0;
                g.is_movable[Obj::Chain]=true;
                if g.prop[Obj::Bear]==3 {
                    g.is_movable[Obj::Bear]=false;
                } else {
                    g.prop[Obj::Bear]=2;
                    g.is_movable[Obj::Bear]=true;
                }
                "The chain is now unlocked."
            }
        }
        Act::Close if g.obj==Obj::Chain && g.loc!=Loc::Barr =>
            "There␣is␣nothing␣here␣to␣which␣the␣chain␣can␣be␣locked.",
        Act::Close if g.obj==Obj::Chain && g.prop[Obj::Chain]!=0 =>
                "It␣was␣already␣locked.",
        Act::Close if g.obj==Obj::Chain => {
            g.prop[Obj::Chain]=2;
            g.is_movable[Obj::Chain] = false;
            if g.toting(Obj::Chain) {
                g.drop(Obj::Chain, g.loc);
            }
            "The␣chain␣is␣now␣locked."
        }
        Act::Open | Act::Close if g.obj==Obj::Grate && g.closing() =>
            panic_at_closing_time(g),
        Act::Open | Act::Close if g.obj==Obj::Grate => {
            g.k = g.prop[Obj::Grate] as i32;
            g.prop[Obj::Grate]=if g.verb==Act::Open {1} else {0};
            match g.k+2*g.prop[Obj::Grate] as i32 {
                0 => "It was already locked.", 
                1 => "The grate is now locked.", 
                2 => "The grate is now unlocked.", 
                3 => "It was already unlocked.", 
                _ => panic!("can't happen"),
            }
        }
        Act::Open | Act::Close => {
           match g.obj {
            Obj::Keys => "You can't lock or unlock the keys.",
            Obj::Cage => "It has no lock",
            Obj::Door if g.prop[Obj::Door]!=0 => default_msg(Act::Relax),
            Obj::Door  => "The door is extremely rusty and refuses to open.", 
            _ => default_msg(g.verb),
            }
        }
        Act::Read if g.dark() => return Goto::CantSeeIt,
        Act::Read if g.obj==Obj::Mag =>
           "I'm afraid the magazine is written in dwarvish.",
        Act::Read if g.obj==Obj::Tablet =>
           "\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\""  ,
        Act::Read if g.obj==Obj::Message=>
           "This is not the maze where the pirate hides his treasure chest.",
        Act::Read if g.obj==Obj::Oyster && g.hinted[1] && g.toting(Obj::Oyster)=>
           "It says the same thing it did before.",
        Act::Read if g.obj==Obj::Oyster && g.toting(Obj::Oyster)=> {
            g.offer(1);
            return Goto::Minor;
        }
        _ => default_msg(g.verb),
    };
    if s != "" {
        println!("{s}");
    }

    Goto::Minor
}

fn intransitive(g: &mut Game) -> Goto {
    g.k = 0;
    match g.verb {
        Act::Go | Act::Relax => {
            print_default_msg(g.verb);
            return Goto::Minor;
        }
        Act::On | Act::Off | Act::Pour | Act::Fill | Act::Drink | Act::Blast | Act::Kill => {
            return Goto::Transitive
        }
        //⟨ Handle cases of intransitive verbs and continue 92 ⟩ ≡
        Act::Take if g.l2o[g.loc].len() == 1 && !g.dwarf() => {
            g.obj = g.l2o[g.loc][0];
            return Goto::Transitive;
        }
        Act::Eat if g.here(Obj::Food) => {
            g.obj = Obj::Food;
            return Goto::Transitive;
        }
        Act::Open | Act::Close => {
            if g.is_here(Obj::Grate) {
                g.obj = Obj::Grate
            } else if g.is_here(Obj::Door) {
                g.obj = Obj::Door
            } else if g.here(Obj::Clam) {
                g.obj = Obj::Clam
            } else if g.here(Obj::Oyster) {
                g.obj = Obj::Oyster
            }
            if g.here(Obj::Chain) {
                if g.obj != Obj::Nothing {
                    return Goto::GetObject;
                } else {
                    g.obj = Obj::Chain
                }
            }
            if g.obj == Obj::Nothing {
                println!("There is nothing here with a lock!");
                return Goto::Minor;
            } else {
                return Goto::Transitive;
            }
        }
        Act::Read if !g.dark() => {
            match (g.here(Obj::Mag), g.here(Obj::Tablet), g.here(Obj::Message)) {
                (true, false, false) => g.obj = Obj::Mag,
                (false, true, false) => g.obj = Obj::Tablet,
                (false, false, true) => g.obj = Obj::Message,
                (_, _, _) => return Goto::GetObject,
            }
            if g.closed && g.toting(Obj::Oyster) {
                g.obj = Obj::Oyster;
            }
            if g.obj != Obj::Nothing {
                return Goto::Transitive;
            } else {
                return Goto::GetObject;
            }
        }
        Act::Inventory => {
            if g.l2o[Loc::Inhand].len() == 0 {
                println!("You're not carrying anything.");
            } else {
                println!("You are currently holding the following:");
                for o in g.l2o[Loc::Inhand].iter() {
                    match *o {
                        Obj::Bear => (),
                        Obj::Cage => {
                            println!(" {}", object_name(*o));
                            if g.prop[Obj::Bird] == 1 {
                                println!(" {}", object_name(Obj::Bird))
                            }
                        }
                        Obj::Bottle => {
                            println!(" {}", object_name(*o));
                            match g.prop[Obj::Bottle] {
                                0 => println!(" {}", object_name(Obj::Water)),
                                1 => (),
                                2 => println!(" {}", object_name(Obj::Oil)),
                                _ => panic!("Not valid"),
                            }
                        }
                        _ => println!(" {}", object_name(*o)),
                    }
                }
            }
            if g.toting(Obj::Bear) {
                println!("You are being followed by a very large, tame bear.");
            }
            return Goto::Minor;
        }
        Act::Brief => {
            g.interval = 10000;
            g.look_count = 3;
            println!("Okay, from now on I'll only describe a place in full the first time you come to it. To get the full description, say \"LOOK\".");
            return Goto::Minor;
        }
        Act::Score => {
            println!(
                "If you were to quit now, you would score {} out of a possible {MAX_SCORE}.",
                score(g) - 4
            );
            if !yes("Do you indeed wish to quit now?", OK, OK) {
                return Goto::Minor;
            } else {
                return Goto::GiveUp;
            }
        }
        Act::Quit => {
            if !yes("Do you really wish to quit now?", OK, OK) {
                return Goto::Minor;
            } else {
                return Goto::Quit;
            }
        }
        Act::Feefie => {
            //TODO - end guard?
            while g.words[0] == INCANTATION[g.k as usize] {
                g.k += 1;
            }
            if g.foobar == -g.k {
                //⟨ Proceed foobarically 139 ⟩ ≡
                g.foobar = g.k + 1;
                if g.foobar != 4 {
                    print_default_msg(Act::Relax);
                    return Goto::Minor;
                }
                g.foobar = 0;
                if g.is_at(Obj::Eggs, Loc::Giant) || (g.toting(Obj::Eggs) && g.loc == Loc::Giant) {
                    print_default_msg(Act::Wave); // nada sucede
                    return Goto::Minor;
                }
                if g.is_at(Obj::Eggs, Loc::Limbo)
                    && g.is_at(Obj::Troll, Loc::Limbo)
                    && g.prop[Obj::Troll] == 0
                {
                    g.prop[Obj::Troll] = 1;
                }
                g.k = match (g.loc == Loc::Giant, g.here(Obj::Eggs)) {
                    (true, _) => 0,
                    (_, true) => 1,
                    (_, false) => 2,
                };
                g.remove(Obj::Eggs);
                g.l2o[Loc::Giant].push(Obj::Eggs);
                println!("{}", object_notes(Obj::Eggs, g.k as i8));
                return Goto::Minor;
            }
            if g.foobar == 0 {
                print_default_msg(Act::Wave); // nada sucede
                return Goto::Minor;
            }
            println!("What's the matter, can't you read? Now you'd best start over.");
            return Goto::Minor;
        }
        _ => {
            println!("{} what?", g.words[0]);
            return Goto::Cycle;
        }
    }

    Goto::Minor  
}

fn score(g: &Game) -> i32 {
    let mut s: i32 = 2;

    if g.dflag > 0 {
        s += 25
    }

    for o in TREASURES {
        if g.prop[o] >= 0 {
            s += 2; // you’ve gotten well inside
            if g.is_at(o, Loc::House) && g.prop[o] == 0 {
                s += match (o as usize).cmp(&(Obj::Chest as usize)) {
                    Ordering::Less => 10,
                    Ordering::Equal => 12,
                    Ordering::Greater => 14,
                }
            }
        }
    }
    s += 10 * (MAX_DEATHS - g.death_count) as i32;
    if !g.gave_up {
        s += 4
    };
    if g.is_at(Obj::Mag, Loc::Witt) {
        s += 1; // proof of your visit
    }
    if g.closing() {
        s += 25
    };
    s += g.bonus;

    for j in 0..g.hinted.len() {
        if g.hinted[j] {
            s -= HINT_COST[j];
        }
    }
    s
}

fn commence(g: &mut Game) -> Goto {
    //⟨ Report the current state 86 ⟩ ≡
    if g.loc == Loc::Limbo {
        return Goto::Death;
    }
    let s = if g.dark() && !is_forced(g.loc) {
        if g.was_dark && pct(35) {
            return Goto::PitchDark;
        }
        PITCH_DARK_MSG
    } else if short_desc(g.loc) != "" || g.visits() % g.interval == 0 {
        long_desc(g.loc)
    } else {
        short_desc(g.loc)
    };

    if g.toting(Obj::Bear) {
        println!("You are being followed by a very large, tame bear.");
    }

    if s != "" {
        println!("\n{s}");
    }

    if is_forced(g.loc) {
        return Goto::TryMove;
    }

    //⟨ Give optional plugh hint 157 ⟩ ≡
    if g.loc == Loc::Y2 && pct(25) && !g.closing() {
        println!("A hollow voice says \"PLUGH\".")
    }
    if !g.dark() {
        //⟨ Describe the objects at this location 88 ⟩ ≡
        g.visits_inc();
        for o in g.l2o[g.loc].clone() {
            if g.prop[o] < 0 {
                // treasure
                if !g.closed {
                    let p = if matches!(o, Obj::Rug | Obj::Chain) {
                        g.prop[o] = 1;
                    } else {
                        g.prop[o] = 0;
                    };
                    g.tally -= 1;
                    //⟨ Zap the lamp if the remaining treasures are too elusive 183 ⟩ ≡
                    if g.tally == g.lost_treasures && g.tally > 0 && g.limit > 35 {
                        g.limit = 35
                    }
                }
            }
            if o != Obj::Treads || g.toting(Obj::Gold) {
                let s = if o == Obj::Treads && g.loc == Loc::Emist {
                    object_notes(o, g.prop[o] + 1)
                } else {
                    object_notes(o, g.prop[o])
                };
                if s != "" {
                    println!("{s}");
                }
            }
        }
    }

    Goto::Minor
}

fn try_move(g: &mut Game) -> Goto {
    //⟨ Handle special motion words 140 ⟩ ≡
    g.newloc = g.loc; // by default we will stay put

    let s=match g.mot {
    Mot::Nowhere => "", 
    Mot::Look => {
        // ⟨ Repeat the long description and continue 141 ⟩ ≡
        g.look_count+=1;
        g.was_dark=false;
        g.visits_zero();
        if g.look_count <= 3 {
            "Sorry, but I am not allowed to give more detail. I will repeat the long description of your location."
        } else {
            ""
        }
    }
    Mot::Cave if g.loc<MIN_IN_CAVE =>
                "I can't see where the cave is, but hereabouts no stream can run on the surface for long... I would try the stream.",
    Mot::Cave => "I need more detailed instructions to do that.",
    Mot::Back => {
            //⟨ Try to go back 143 ⟩ ≡ 
            let l=if is_forced(g.loc) {g.oldoldloc} else {g.oldloc};
            g.oldoldloc = g.oldloc;
            g.oldloc = g.loc;
            if l == g.loc {
                "Sorry, but I no longer seem to remember how you got here."
            } else {
                let dest=g.lookup_instructions();
                if dest==Loc::Nowhere {
                    "You can't get there from here." 
                } else {
                    g.newloc=dest;
                    return Goto::GoForIt
                }
            }
       }
    _ => {
           g.newloc=g.lookup_instructions();
           return Goto::GoForIt
         }
    };
    if s != "" {
        println!("{s}")
    }
    Goto::Major
}

fn go_for_it(g: &mut Game) -> Goto {
    g.oldoldloc = g.oldloc;
    g.oldloc = g.loc;

    let s = match g.newloc {
        Loc::Nowhere => {
            //⟨ Report on inapplicable motion and continue 148 ⟩ ≡
            g.newloc = g.loc;
            match g.mot {
            Mot::Crawl => "Which way?",
            Mot::Xyzzy | Mot::Plugh => default_msg(Act::Wave),
            Mot::In | Mot::Out =>
                  "I don't know in from out here. Use compass points or name something in the general direction you want to go.",
            Mot::Forward | Mot::L | Mot::R =>
                   "I am unsure how you are facing. Use compass points or nearby objects.",
            _ if g.mot<=Mot::Forward => "There is no way to go in that direction.",
            _ if matches!(g.verb, Act::Find | Act::Inventory) => default_msg(Act::Find),
            _ => "I don’t know how to apply that word here."
            }
        }
        Loc::Sayit0
        | Loc::Sayit1
        | Loc::Sayit2
        | Loc::Sayit3
        | Loc::Sayit4
        | Loc::Sayit5
        | Loc::Sayit6
        | Loc::Sayit7
        | Loc::Sayit8
        | Loc::Sayit9
        | Loc::Sayit10
        | Loc::Sayit11
        | Loc::Sayit12 => {
            g.newloc = g.loc;
            SAYIT[g.loc as usize - Loc::Sayit0 as usize]
        }
        Loc::Ppass if g.holding() == 0 || (g.holding() == 1 && g.toting(Obj::Emerald)) =>
        //⟨ Choose newloc via plover-alcove passage 149 ⟩ ≡
        {
            g.newloc = if g.loc == Loc::Proom {
                Loc::Alcove
            } else {
                Loc::Proom
            };
            ""
        }
        Loc::Ppass => {
            g.newloc = g.loc; // stay
            "Something you’re carrying won’t fit through the tunnel with you. You’d best take inventory and drop something."
        }

        Loc::Pdrop => {
            //⟨ Drop the emerald during plover transportation 150 ⟩=;
            g.drop(Obj::Emerald, g.loc);
            g.newloc = Loc::Proom;
            ""
        }
        Loc::Troll if g.prop[Obj::Troll] == 1 => {
            //⟨ Block the troll bridge and stay put 152 ⟩ ≡
            g.remove(Obj::Troll);
            g.put(Obj::Troll, Loc::Swside);
            g.put(Obj::Troll, Loc::Neside);
            g.prop[Obj::Troll] = 0;
            g.remove(Obj::Troll2);
            g.remove(Obj::Bridge);
            g.put(Obj::Bridge, Loc::Swside);
            g.put(Obj::Bridge, Loc::Neside);
            g.newloc = g.loc;
            object_notes(Obj::Troll, 1)
        }
        Loc::Troll => {
            //⟨ Cross troll bridge if possible 151 ⟩ ≡
            g.newloc = if g.loc == Loc::Neside {
                Loc::Swside
            } else {
                Loc::Neside
            };
            if g.prop[Obj::Troll] == 0 {
                g.prop[Obj::Troll] = 1;
            }
            if g.toting(Obj::Bear) {
                println!("Just as you reach the other side, the bridge buckles beneath the weight of the bear, who was still following you around. You scrabble desperately for support, but as the bridge collapses you stumble back and fall into the chasm.");
                g.prop[Obj::Bridge] = 1;
                g.prop[Obj::Troll] = 2;
                g.drop(Obj::Bear, g.newloc);
                g.is_movable[Obj::Bear] = false;
                g.prop[Obj::Bear] = 3; // the bear is dead
                for o in [Obj::Spices, Obj::Chain] {
                    //if g.prop[o] < 0 && g.place(o) >= Loc::Neside {
                    if g.prop[o] < 0 && g.place(o).iter().any(|p| *p >= Loc::Neside) {
                        g.lost_treasures += 1;
                    }
                }
                g.oldoldloc = g.newloc; // if you are revived, you got across
                return Goto::Death;
            }
            ""
        }
        _ => "",
    };
    if s != "" {
        println!("{s}")
    }

    return Goto::Major;
}

fn main() {
    let mut g = Game::new();

    g.offer(0);
    g.limit = if g.hinted[0] { 1000 } else { 330 };

    let mut state = Goto::Major;
    loop {
        state = match state {
            Goto::DwarvesUpset => dwarves_upset(),
            Goto::Branch => branch(&mut g),
            Goto::GetObject => get_object(&g),
            Goto::TryMove => try_move(&mut g),
            Goto::GiveUp => major(&mut g),
            Goto::CantSeeIt => cant_see_it(&mut g),
            Goto::Major => major(&mut g),
            Goto::Minor => minor(&mut g),
            Goto::Cycle => cycle(&mut g),
            Goto::Death => death(&mut g),
            Goto::PitchDark => pitch_dark(&mut g),
            Goto::Quit => quit(&g),
            Goto::Commence => commence(&mut g),
            Goto::Transitive => transitive(&mut g),
            Goto::Intransitive => intransitive(&mut g),
            Goto::Parse => parse(&mut g),
            Goto::PreParse => pre_parse(&mut g),
            Goto::GoForIt => go_for_it(&mut g),
        }
    }
}
