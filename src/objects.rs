use std::ops::{Index, IndexMut};

#[rustfmt::skip]
#[derive(Eq, PartialEq, PartialOrd, Hash, Copy, Clone, Debug)]
pub enum Obj {
    Nothing, Keys, Lamp, Grate, Cage, Age, Rod, Rod2, Treads, 
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
