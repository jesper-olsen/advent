use std::ops::{Index, IndexMut};

#[rustfmt::skip]
#[derive(Eq, PartialEq, PartialOrd, Hash, Copy, Clone, Debug)]
pub enum Obj {
    Nothing, Keys, Lamp, Grate, Cage, Rod, Rod2, Treads, 
    Bird, Door, Pillow, Snake, Crystal, 
    Tablet, Clam, Oyster, Mag, Dwarf, Knife, Food, Bottle, Water, Oil,
    Mirror, Plant, Plant2, Stalactite, Shadow, 
    Axe, Art, Pirate, Dragon, Bridge, Troll, Troll2, 
    Bear, Message, Geyser, Pony, Batteries, Moss, Gold, Diamonds, Silver,
    Jewels, Coins, Chest, Eggs, Trident, Vase, Emerald, Pyramid, Pearl,
    Rug, Spices, Chain,
}

use Obj::*;
pub const N_OBJECTS: usize = Chain as usize + 1;

#[rustfmt::skip]
pub const TREASURES: [Obj;15] = [ Gold, Diamonds, Silver, Jewels, Coins, Chest, 
    Eggs, Trident, Vase, Emerald, Pyramid, Pearl, Rug, Spices, Chain];

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

impl Obj {
    pub fn treasure(self) -> bool {
        self >= Obj::Gold
    }

    pub fn name(self) -> &'static str {
        match self {
            Rug => "Persian rug",
            Chain => "Golden chain",
            Spices => "Rare spices",
            Pearl => "Glistening pearl",
            Pyramid => "Platinum pyramid",
            Emerald => "Egg-sized emerald",
            Vase => "Ming vase",
            Trident => "Jeweled trident \u{1F531}",
            Eggs => "Golden eggs",
            Chest => "Treasure chest",
            Coins => "Rare coins",
            Jewels => "Precious jewelry",
            Silver => "Bars of silver",
            Diamonds => "Several diamonds",
            Gold => "Large gold nugget",
            Batteries => "Batteries",
            Axe => "Dwarf's axe \u{1FA93}",
            Oil => "Oil in the bottle",
            Water => "Water in the bottle",
            Bottle => "Small bottle.",
            Food => "Tasty food.",
            Mag => "Spelunker Today",
            Oyster => "Giant oyster >GROAN!<",
            Clam => "Giant clam >GRUNT!<",
            Pillow => "Velvet pillow",
            Bird => "Little bird in a cage",
            Rod2 => "Black rod \u{1F9E8}",
            Rod => "Black rod",
            Cage => "Wicker cage",
            Lamp => "Brass lantern",
            Keys => "Set of keys",
            _ => "",
        }
    } // name

    pub const fn note(self, prop: i8) -> &'static str {
        match (self,prop) {
            (Rug,0) => "There is a Persian rug spread out on the floor!",
            (Rug,1) => "The dragon is sprawled out on a Persian rug!!",
            (Troll2,0) => "The troll is nowhere to be seen.",
            (Troll,0) => "A burly troll stands by the bridge and insists you throw him a treasure before you may cross. \u{1F47A}",
            (Troll,1) => "The troll steps out from beneath the bridge and blocks your way. \u{1F47A}",
            (Bridge,0) => "A rickety wooden bridge extends across the chasm, vanishing into the mist. A sign posted on the bridge reads, \"STOP! PAY TROLL!\"",
            (Bridge,1) => "The wreckage of a bridge (and a dead bear) can be seen at the bottom of the chasm.",
            (Dragon,0) => "A huge green fierce dragon bars the way! \u{1F432}",
            (Dragon,1) => "The body of a huge green dead dragon is lying off to one side.",
            (Shadow,0) => "The shadowy figure seems to be trying to attract your attention.",
            (Plant2,0) => "The top of a 12-foot-tall beanstalk is poking out of the west pit.",
            (Plant2,1) => "There is a huge beanstalk growing out of the west pit up to the hold.",
            (Crystal,0) => "",
            (Crystal,1) => "A crystal bridge now spans the fissure.",
            (Crystal,2) => "The crystal bridge has vanished!",
            (Treads,0) => "Rough stone steps lead down the pit.",
            (Treads,1) => "Rough stone steps lead up the dome.",
            (Grate,0) => "The grate is locked.",
            (Grate,1) => "The grate is open.",
            (Chain,0) => "There is a golden chain lying in a heap on the floor!",
            (Chain,1) => "The bear is locked to the wall with a golden chain!",
            (Chain,2) => "There is a golden chain locked to the wall!",
            (Spices,0) => "There are rare spices here!",
            (Pearl,0) => "Off to one side lies a glistening pearl!",
            (Pyramid,0) => "There is a platinum pyramid here, 8 inches on a side!",
            (Emerald,0) => "There is an emerald here the size of a plover's egg!",
            (Vase,0) => "There is a delicate, precious, Ming vase here!",
            (Vase,1) => "The floor is littered with worthless shards of pottery.",
            (Trident,0) => "There is a jewel-encrusted trident here! \u{1F531}",
            (Eggs,0) => "There is a large nest here, full of golden eggs!",
            (Eggs,1) => "The nest of golden eggs has vanished!",
            (Eggs,2) => "Done!",
            (Chest,0) => "The pirate's treasure chest is here!",
            (Coins,0) => "There are many coins here! \u{1FA99}",
            (Jewels,0) => "There is precious jewelry here! \u{1F48D}",
            (Silver,0) => "There are bars of silver here",
            (Diamonds,0) => "There are diamonds here! \u{1F48E}",
            (Gold,0) => "There is a large sparkling nugget of gold here!",
            (Batteries,0) => "There are fresh batteries here. \u{1F50B}",
            (Batteries,1) => "Some worn-out batteries have been discarded nearby. \u{1F50B}",
            (Pony,0) => "There is a massive vending machine here. The instructions on it read: Drop coins here to receive fresh batteries.",
            (Message,0) => "There is a message scrawled in the dust in a flowery script, reading: \"This is not the maze where the pirate hides his treasure chest.\"",
            (Bear,0) => "There is a ferocious cave bear eying you from the far end of the room!",
            (Bear,1) => "There is a gentle cave bear sitting placidly in one corner",
            (Bear,2) => "There is a contented-looking bear wandering about nearby",
            (Axe,0) => "There is a little axe here. \u{1FA93}",
            (Axe,1) => "There is a little axe lying beside the bear. \u{1FA93}",
            (Plant,0) => "There is a tiny little plant in the pit, murmuring \'Water, water, ...\" \u{1F33F}",
            (Plant,1) => "The plant spurts into furious growth for a few seconds. \u{1F33F}",
            (Plant,2) => "There is a 12-foot-tall beanstalk stretching up out of the pit, bellowing \"Water!! Water!!\" \u{1F33F}",
            (Plant,3) => "The plant grows explosively, almost filling the bottom of the pit. \u{1F33F}",
            (Plant,4) => "There is a gigantic beanstalk stretching all the way up to the hole. \u{1F33F}",
            (Plant,5) => "You've over-watered the plant! It's shriveling up! It's it's... \u{1F33F}",
            (Bottle,0) => "There is a bottle of water here.",
            (Bottle,1) => "There is an empty bottle here.",
            (Bottle,2) => "There is a bottle of oil here.",
            (Food,0) => "There is food here.",
            (Mag,0) => "There are a few recent issues of \"Spelunker Today\" magazine here.",
            (Oyster,0) => "There is an enormous oyster here with its shell tightly closed.",
            (Oyster,1) => "Interesting. There seems to be something written on the underside of the oyster.",
            (Clam,0) => "There is an enourmous clam here with its shell tightly closed",
            (Tablet,0) => "A massive stone tablet embedded in the wall reads: \n\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\"",
            (Snake,0) => "A huge green fierce snake bars the way! \u{1F40D}",
            (Pillow,0) => "A small velvet pillow lies on the floor.",
            (Door,0) => "The way north is barred by a massive, rusty, iron door.",
            (Door,1) => "The way north leads through a massive, rusty, iron door",
            (Bird,0) => "A cheerful little bird is sitting here singing. \u{1F99C}",
            (Bird,1) => "There is a little bird in the cage.",
            (Rod2,0) => "A three-foot black rod with a rusty mark on an end lies nearby. \u{1F9E8}",
            (Rod,0) => "A three-foot black rod with a rusty star on an end lies nearby.",
            (Cage,0) => "There is a small wicker cage discarded nearby",
            (Lamp,0) => "There is a shiny brass lamp nearby.",
            (Lamp,1) => "There is a lamp shining nearby. \u{1F3EE}",
            (Keys,0) => "There are some keys on the ground here.",
            _ => "",
        }
    }
}
