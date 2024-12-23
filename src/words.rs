use crate::actions;
use crate::motions;
use crate::objects;
use actions::*;
use motions::*;
use objects::*;

#[derive(PartialEq, Debug)]
pub enum Word {
    None,
    Motion(Mot),
    Object(Obj),
    Action(Act),
    Message(&'static str),
}

impl Word {
    pub fn from_string(s: &str) -> Self {
        match s {
        "north" | "n" => Word::Motion(Mot::N),
        "south" | "s" => Word::Motion(Mot::S),
        "east" | "e" => Word::Motion(Mot::E),
        "west" | "w" => Word::Motion(Mot::W),
        "ne" => Word::Motion(Mot::NE),
        "nw" => Word::Motion(Mot::NW),
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
}
