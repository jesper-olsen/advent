use rand::random;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::io::{self, Write};
use std::process;

pub mod motions;
use motions::*;
pub mod objects;
use objects::*;
pub mod actions;
use actions::*;
pub mod words;
use words::*;
pub mod locations;
use locations::*;

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

const MAX_DEATHS: usize = 3;
static DEATH_WISHES: [&str; 2*MAX_DEATHS] = [
"Oh dear, you seem to have gotten yourself killed. I might be able to help you out, but I've never really done this before. Do you want me to try to reincarnate you?", 
"All right. But don't blame me if something goes wr......  −−− POOF!! −−− You are engulfed in a cloud of orange smoke. Coughing and gasping, you emerge from the smoke and find....",
"You clumsy oaf, you've done it again! I don't know how long I can keep this up. Do you want me to try reincarnating you again?",
"Okay, now where did I put my resurrection␣kit?....  >POOF!< Everything disappears in a dense cloud of orange smoke.",
"Now you've really done it! I'm out of orange smoke! You don't expect me to do a decent reincarnation without any orange smoke, do you?",
"Okay, if you're so smart, do it yourself! I'm leaving!"];

fn debug(g: &mut Game, m: &str) {
    let v: Vec<Mot> = MOTIONS
        .into_iter()
        .filter(|&m| g.travel(g.loc, m, false) != Loc::Limbo)
        .collect();
    println!(
        "###{m} l: {:?} t: {} Rod2:{:?}, clock1:{} clock2:{}",
        g.loc,
        g.tally,
        g.place(Obj::Rod2),
        g.clock1,
        g.clock2
    );
    println!("###{m} motions: {v:?}")
}

fn shortest_route(g: &Game, x: Loc, y: Loc) {
    // calc shortest route from x to y - note that
    // some transitions are probabilstic or depend
    // on object locations/properties, so...
    // also, many motions are redundant ...
    println!("Shortst Route {x:?} -> {y:?}");
    let mut used = [false; N_LOC];
    let mut prev = [(Mot::Nowhere, Loc::Limbo); N_LOC];
    let mut q = VecDeque::from([x]);
    used[x] = true;

    while let Some(s) = q.pop_front() {
        if s == y {
            // trace back
            let mut v = vec![];
            let mut s = y;
            loop {
                let (m, p) = prev[s];
                if p == Loc::Limbo {
                    break;
                }
                v.push((m, p));
                s = p;
            }
            while let Some((m, p)) = v.pop() {
                println!("{p:?} ({m:?})->");
            }
            println!("{y:?}");
            return;
        }
        for m in MOTIONS {
            let u = g.travel(s, m, false);
            if u != Loc::Limbo && !is_forced(u) && !used[u] {
                q.push_back(u);
                used[u] = true;
                prev[u] = (m, s);
            }
        }
    }
}

fn printif(s: &str) {
    if s != "" {
        println!("{s}")
    }
}

fn get_input() -> String {
    let mut s = String::new();
    io::stdin().read_line(&mut s).ok();
    String::from(s.trim())
}

fn yes(q: &str, y: &str, n: &str) -> bool {
    loop {
        print!("{q}\n** ");
        let _ = io::stdout().flush();
        if let Some(c) = get_input().chars().next() {
            match c {
                'Y' | 'y' => {
                    printif(y);
                    return true;
                }
                'N' | 'n' => {
                    printif(n);
                    return false;
                }
                _ => println!(" Please answer Yes or No."),
            }
        }
    }
}

fn listen(g: &Game) -> Vec<String> {
    loop {
        print!("[{:?}] * ", g.loc);
        let _ = io::stdout().flush();
        let words: Vec<String> = get_input()
            .to_lowercase()
            .split_whitespace()
            .map(|s| s.to_string())
            .map(|s| s.chars().take(5).collect())
            .collect();

        //println!("### [{:?}] * {}", g.loc, words.join(" "));

        match words.len() {
            0 => println!(" Tell me to do something."),
            1 | 2 => return words,
            _ => println!(" Please stick to 1− and 2−word commands."),
        }
    }
}

const N_HINTS: usize = 8;
const HINT_COST: [i32; N_HINTS] = [5, 10, 2, 2, 2, 4, 5, 3];

static HINT: [(&str,&str); N_HINTS] = [
   ("Welcome to Adventure!! Would you like instructions?", 
   "Somewhere nearby is Colossal Cave, where others have found fortunes in \
   treasure and gold, though it is rumored that some who enter are never \
   seen again. Magic is said to work in the cave. I will be your eyes \
   and hands. Direct me with commands of one or two words. I should \
   warn you that I look at only the first five letters of each word, so \
   you'll have to enter \"NORTHEAST\" as \"NE\" to distinguish it from \
   \"NORTH\". Should you get stuck, type \"HELP\" for some general hints. \
   For information on how to end your adventure, etc., type \"INFO\". \
   \n                        −  −  −\nThe first adventure program was \
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

fn pct(n: u8) -> bool {
    random::<u8>() % 100 < n
}

fn ran(n: usize) -> usize {
    random::<usize>() % n
}

impl Game {
    fn travel(&self, loc: Loc, mot: Mot, dwarf: bool) -> Loc {
        use Mot::*;
        match (loc, mot) {
            (Loc::Road, W | U | Road) => Loc::Hill,
            (Loc::Road, E | In | House | Enter) => Loc::House,
            (Loc::Road, S | D | Gully | Stream | Downstream) => Loc::Valley,
            (Loc::Road, N | Woods) => Loc::Forest,
            (Loc::Road, Depression) => Loc::Woods,

            (Loc::Hill, Road | House | Forward | E | D) => Loc::Road,
            (Loc::Hill, Woods | N | S) => Loc::Forest,

            (Loc::House, Enter | Out | Outdoors | W) => Loc::Road,
            (Loc::House, Xyzzy) => Loc::Debris,
            (Loc::House, Plugh) => Loc::Y2,
            (Loc::House, Downstream | Stream) => Loc::Sewer,

            (Loc::Valley, Upstream | House | N) => Loc::Road,
            (Loc::Valley, Woods | E | W | U) => Loc::Forest,
            (Loc::Valley, Downstream | S | D) => Loc::Slit,
            (Loc::Valley, Depression) => Loc::Outside,

            (Loc::Forest, Valley | E | D) => Loc::Valley,
            (Loc::Forest, Woods | Forward | N) if pct(50) => Loc::Forest,
            (Loc::Forest, Woods | Forward | N) => Loc::Woods,
            (Loc::Forest, W | S) => Loc::Forest,

            (Loc::Woods, Road | N) => Loc::Road,
            (Loc::Woods, Valley | E | W | D) => Loc::Valley,
            (Loc::Woods, Woods | S) => Loc::Forest,

            (Loc::Slit, House) => Loc::Road,
            (Loc::Slit, Upstream | N) => Loc::Valley,
            (Loc::Slit, Woods | E | W) => Loc::Forest,
            (Loc::Slit, Downstream | Rock | Bed | S) => Loc::Outside,
            (Loc::Slit, Slit | Stream | D) => Loc::Sayit0,

            (Loc::Outside, Woods | E | W | S) => Loc::Forest,
            (Loc::Outside, House) => Loc::Road,
            (Loc::Outside, Upstream | Gully | N) => Loc::Slit,
            (Loc::Outside, Enter | In | D) if self.prop[Obj::Grate] != 0 => Loc::Inside,
            (Loc::Outside, Enter | In | D) => Loc::Sayit0,

            (Loc::Inside, Out | U) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Inside, Out | U) => Loc::Sayit1,
            (Loc::Inside, Crawl | Cobbles | In | W) => Loc::Cobbles,
            (Loc::Inside, Pit) => Loc::Spit,
            (Loc::Inside, Debris) => Loc::Debris,

            (Loc::Cobbles, Out | Surface | Nowhere | E) => Loc::Inside,
            (Loc::Cobbles, In | Dark | W | Debris) => Loc::Debris,
            (Loc::Cobbles, Pit) => Loc::Spit,

            (Loc::Debris, Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Debris, Entrance) => Loc::Inside,
            (Loc::Debris, Crawl | Cobbles | Passage | Low | E) => Loc::Cobbles,
            (Loc::Debris, Canyon | In | U | W) => Loc::Awk,
            (Loc::Debris, Xyzzy) => Loc::House,
            (Loc::Debris, Pit) => Loc::Spit,

            (Loc::Awk, Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Awk, Entrance) => Loc::Inside,
            (Loc::Awk, D | E | Debris) => Loc::Debris,
            (Loc::Awk, In | U | W) => Loc::Bird,
            (Loc::Awk, Pit) => Loc::Spit,

            (Loc::Bird, Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Bird, Entrance) => Loc::Inside,
            (Loc::Bird, Debris) => Loc::Debris,
            (Loc::Bird, Canyon | E) => Loc::Awk,
            (Loc::Bird, Passage | Pit | W) => Loc::Spit,

            (Loc::Spit, Depression) if self.prop[Obj::Grate] != 0 => Loc::Outside,
            (Loc::Spit, Entrance) => Loc::Inside,
            (Loc::Spit, Debris) => Loc::Debris,
            (Loc::Spit, Passage | E) => Loc::Bird,
            (Loc::Spit, D | Pit | Steps) if !dwarf && self.toting(Obj::Gold) => Loc::Neck,
            (Loc::Spit, D) if !dwarf && !self.toting(Obj::Gold) => Loc::Emist,
            (Loc::Spit, Crack | W) => Loc::Crack,

            (Loc::Emist, L | S) => Loc::Nugget,
            (Loc::Emist, Forward | Hall | W) => Loc::Efiss,
            (Loc::Emist, Stairs | D | N) => Loc::Hmk,
            (Loc::Emist, U | Pit | Steps | Dome | Passage | E)
                if !dwarf && self.toting(Obj::Gold) =>
            {
                Loc::Cant
            }
            (Loc::Emist, U) => Loc::Spit,
            (Loc::Emist, Y2) => Loc::Jumble,

            (Loc::Nugget, Hall | Out | N) => Loc::Emist,

            (Loc::Efiss, Hall | E) => Loc::Emist,

            (Loc::Efiss, Jump) if self.prop[Obj::Crystal] != 0 => Loc::Sayit2,
            (Loc::Efiss, Forward) if self.prop[Obj::Crystal] != 1 => Loc::Lose,
            (Loc::Efiss, Over | Across | W | Cross) if self.prop[Obj::Crystal] != 1 => Loc::Sayit3,
            (Loc::Efiss, Over | Across | W | Cross) => Loc::Wfiss,

            (Loc::Wfiss, Jump) if self.prop[Obj::Crystal] != 0 => Loc::Sayit2,

            (Loc::Wfiss, Forward) if self.prop[Obj::Crystal] != 1 => Loc::Lose,
            (Loc::Wfiss, Over | Across | E | Cross) if self.prop[Obj::Crystal] != 1 => Loc::Sayit3,
            (Loc::Wfiss, Over) => Loc::Efiss,
            (Loc::Wfiss, N) => Loc::Thru,
            (Loc::Wfiss, W) => Loc::Wmist,

            (Loc::Wmist, S | U | Passage | Climb) => Loc::Like1,
            (Loc::Wmist, E) => Loc::Wfiss,
            (Loc::Wmist, N) => Loc::Duck,
            (Loc::Wmist, W | Crawl) => Loc::Elong,

            (Loc::Like1, U) => Loc::Wmist,
            (Loc::Like1, N) => Loc::Like1,
            (Loc::Like1, E) => Loc::Like2,
            (Loc::Like1, S) => Loc::Like4,
            (Loc::Like1, W) => Loc::Like11,

            (Loc::Like2, W) => Loc::Like1,
            (Loc::Like2, S) => Loc::Like3,
            (Loc::Like2, E) => Loc::Like4,

            (Loc::Like3, E) => Loc::Like2,
            (Loc::Like3, D) => Loc::Dead5,
            (Loc::Like3, S) => Loc::Like6,
            (Loc::Like3, N) => Loc::Dead9,

            (Loc::Like4, W) => Loc::Like1,
            (Loc::Like4, N) => Loc::Like2,
            (Loc::Like4, E) => Loc::Dead3,
            (Loc::Like4, S) => Loc::Dead4,
            (Loc::Like4, U | D) => Loc::Like14,

            (Loc::Like5, E) => Loc::Like6,
            (Loc::Like5, W) => Loc::Like7,

            (Loc::Like6, E) => Loc::Like3,
            (Loc::Like6, W) => Loc::Like5,
            (Loc::Like6, D) => Loc::Like7,
            (Loc::Like6, S) => Loc::Like8,

            (Loc::Like7, W) => Loc::Like5,
            (Loc::Like7, U) => Loc::Like6,
            (Loc::Like7, E) => Loc::Like8,
            (Loc::Like7, S) => Loc::Like9,

            (Loc::Like8, W) => Loc::Like6,
            (Loc::Like8, E) => Loc::Like7,
            (Loc::Like8, S) => Loc::Like8,
            (Loc::Like8, U) => Loc::Like9,
            (Loc::Like8, N) => Loc::Like10,
            (Loc::Like8, D) => Loc::Like11,

            (Loc::Like9, W) => Loc::Like7,
            (Loc::Like9, N) => Loc::Like8,
            (Loc::Like9, S) => Loc::Like6,

            (Loc::Like10, W) => Loc::Like8,
            (Loc::Like10, N) => Loc::Like10,
            (Loc::Like10, D) => Loc::Dead7,
            (Loc::Like10, E) => Loc::Brink,

            (Loc::Like11, N) => Loc::Like1,
            (Loc::Like11, W | S) => Loc::Like11,
            (Loc::Like11, E) => Loc::Dead1,

            (Loc::Like12, S) => Loc::Brink,
            (Loc::Like12, E) => Loc::Like13,
            (Loc::Like12, W) => Loc::Dead10,

            (Loc::Like13, N) => Loc::Brink,
            (Loc::Like13, W) => Loc::Like12,
            (Loc::Like13, NW) => Loc::Dead2, // dk - a dirty trick!

            (Loc::Like14, U | D) => Loc::Like4,

            (Loc::Brink, D | Climb) => Loc::Bird,
            (Loc::Brink, W) => Loc::Like10,
            (Loc::Brink, S) => Loc::Dead8,
            (Loc::Brink, N) => Loc::Like12,
            (Loc::Brink, E) => Loc::Like13,

            (Loc::Elong, E | U | Crawl | W) => Loc::Wlong,
            (Loc::Elong, N | D | Hole) => Loc::Cross,

            (Loc::Wlong, E) => Loc::Elong,
            (Loc::Wlong, N) => Loc::Cross,
            (Loc::Wlong, S) if !dwarf => Loc::Diff0,

            (Loc::Diff0, S) => Loc::Diff1,
            (Loc::Diff0, SW) => Loc::Diff2,
            (Loc::Diff0, NE) => Loc::Diff3,
            (Loc::Diff0, SE) => Loc::Diff4,
            (Loc::Diff0, U) => Loc::Diff5,
            (Loc::Diff0, NW) => Loc::Diff6,
            (Loc::Diff0, E) => Loc::Diff7,
            (Loc::Diff0, W) => Loc::Diff8,
            (Loc::Diff0, N) => Loc::Diff9,
            (Loc::Diff0, D) => Loc::Wlong,

            (Loc::Diff1, W) => Loc::Diff0,
            (Loc::Diff1, SE) => Loc::Diff2,
            (Loc::Diff1, NW) => Loc::Diff3,
            (Loc::Diff1, SW) => Loc::Diff4,
            (Loc::Diff1, NE) => Loc::Diff5,
            (Loc::Diff1, U) => Loc::Diff6,
            (Loc::Diff1, D) => Loc::Diff7,
            (Loc::Diff1, N) => Loc::Diff8,
            (Loc::Diff1, S) => Loc::Diff9,
            (Loc::Diff1, E) => Loc::Diff10,

            (Loc::Diff2, NW) => Loc::Diff0,
            (Loc::Diff2, U) => Loc::Diff1,
            (Loc::Diff2, N) => Loc::Diff3,
            (Loc::Diff2, S) => Loc::Diff4,
            (Loc::Diff2, W) => Loc::Diff5,
            (Loc::Diff2, SW) => Loc::Diff6,
            (Loc::Diff2, NE) => Loc::Diff7,
            (Loc::Diff2, E) => Loc::Diff8,
            (Loc::Diff2, D) => Loc::Diff9,
            (Loc::Diff2, SE) => Loc::Diff10,

            (Loc::Diff3, U) => Loc::Diff0,
            (Loc::Diff3, D) => Loc::Diff1,
            (Loc::Diff3, W) => Loc::Diff2,
            (Loc::Diff3, NE) => Loc::Diff4,
            (Loc::Diff3, SW) => Loc::Diff5,
            (Loc::Diff3, E) => Loc::Diff6,
            (Loc::Diff3, N) => Loc::Diff7,
            (Loc::Diff3, NW) => Loc::Diff8,
            (Loc::Diff3, SE) => Loc::Diff9,
            (Loc::Diff3, S) => Loc::Diff10,

            (Loc::Diff4, NE) => Loc::Diff0,
            (Loc::Diff4, N) => Loc::Diff1,
            (Loc::Diff4, NW) => Loc::Diff2,
            (Loc::Diff4, SE) => Loc::Diff3,
            (Loc::Diff4, E) => Loc::Diff5,
            (Loc::Diff4, D) => Loc::Diff6,
            (Loc::Diff4, S) => Loc::Diff7,
            (Loc::Diff4, U) => Loc::Diff8,
            (Loc::Diff4, W) => Loc::Diff9,
            (Loc::Diff4, SW) => Loc::Diff10,

            (Loc::Diff5, N) => Loc::Diff0,
            (Loc::Diff5, SE) => Loc::Diff1,
            (Loc::Diff5, D) => Loc::Diff2,
            (Loc::Diff5, S) => Loc::Diff3,
            (Loc::Diff5, E) => Loc::Diff4,
            (Loc::Diff5, W) => Loc::Diff6,
            (Loc::Diff5, SW) => Loc::Diff7,
            (Loc::Diff5, NE) => Loc::Diff8,
            (Loc::Diff5, NW) => Loc::Diff9,
            (Loc::Diff5, U) => Loc::Diff10,

            (Loc::Diff6, E) => Loc::Diff0,
            (Loc::Diff6, W) => Loc::Diff1,
            (Loc::Diff6, U) => Loc::Diff2,
            (Loc::Diff6, SW) => Loc::Diff3,
            (Loc::Diff6, D) => Loc::Diff4,
            (Loc::Diff6, S) => Loc::Diff5,
            (Loc::Diff6, NW) => Loc::Diff7,
            (Loc::Diff6, SE) => Loc::Diff8,
            (Loc::Diff6, NE) => Loc::Diff9,
            (Loc::Diff6, N) => Loc::Diff10,

            (Loc::Diff7, SE) => Loc::Diff0,
            (Loc::Diff7, NE) => Loc::Diff1,
            (Loc::Diff7, S) => Loc::Diff2,
            (Loc::Diff7, D) => Loc::Diff3,
            (Loc::Diff7, U) => Loc::Diff4,
            (Loc::Diff7, NW) => Loc::Diff5,
            (Loc::Diff7, N) => Loc::Diff6,
            (Loc::Diff7, SW) => Loc::Diff8,
            (Loc::Diff7, E) => Loc::Diff9,
            (Loc::Diff7, W) => Loc::Diff10,

            (Loc::Diff8, D) => Loc::Diff0,
            (Loc::Diff8, E) => Loc::Diff1,
            (Loc::Diff8, NE) => Loc::Diff2,
            (Loc::Diff8, U) => Loc::Diff3,
            (Loc::Diff8, W) => Loc::Diff4,
            (Loc::Diff8, N) => Loc::Diff5,
            (Loc::Diff8, S) => Loc::Diff6,
            (Loc::Diff8, SE) => Loc::Diff7,
            (Loc::Diff8, SW) => Loc::Diff9,
            (Loc::Diff8, NW) => Loc::Diff10,

            (Loc::Diff9, SW) => Loc::Diff0,
            (Loc::Diff9, NW) => Loc::Diff1,
            (Loc::Diff9, E) => Loc::Diff2,
            (Loc::Diff9, W) => Loc::Diff3,
            (Loc::Diff9, N) => Loc::Diff4,
            (Loc::Diff9, D) => Loc::Diff5,
            (Loc::Diff9, SE) => Loc::Diff6,
            (Loc::Diff9, U) => Loc::Diff7,
            (Loc::Diff9, S) => Loc::Diff8,
            (Loc::Diff9, NE) => Loc::Diff10,

            (Loc::Diff10, SW) => Loc::Diff1,
            (Loc::Diff10, N) => Loc::Diff2,
            (Loc::Diff10, E) => Loc::Diff3,
            (Loc::Diff10, NW) => Loc::Diff4,
            (Loc::Diff10, SE) => Loc::Diff5,
            (Loc::Diff10, NE) => Loc::Diff6,
            (Loc::Diff10, W) => Loc::Diff7,
            (Loc::Diff10, D) => Loc::Diff8,
            (Loc::Diff10, U) => Loc::Diff9,
            (Loc::Diff10, S) => Loc::Pony,

            (Loc::Pony, N | Out) => Loc::Diff10,

            (Loc::Cross, W) => Loc::Elong,
            (Loc::Cross, N) => Loc::Dead0,
            (Loc::Cross, E) => Loc::West,
            (Loc::Cross, S) => Loc::Wlong,

            (Loc::Hmk, Stairs | U | E) => Loc::Emist,
            (Loc::Hmk, N | L) if self.prop[Obj::Snake] != 0 => Loc::Ns,
            (Loc::Hmk, S | R) if self.prop[Obj::Snake] != 0 => Loc::South,
            (Loc::Hmk, W | Forward) if self.prop[Obj::Snake] != 0 => Loc::West,
            (Loc::Hmk, N) => Loc::Snaked,
            (Loc::Hmk, SW) if pct(35) => Loc::Secret,
            (Loc::Hmk, SW) if self.is_here(Obj::Snake) => Loc::Snaked,
            (Loc::Hmk, Secret) => Loc::Secret,

            (Loc::West, Hall | Out | E) => Loc::Hmk,
            (Loc::West, W | U) => Loc::Cross,

            (Loc::South, Hall | Out | N) => Loc::Hmk,

            (Loc::Ns, Hall | Out | S) => Loc::Hmk,
            (Loc::Ns, N | Y2) => Loc::Y2,
            (Loc::Ns, D | Hole) => Loc::Dirty,

            (Loc::Y2, Plugh) => Loc::House,
            (Loc::Y2, S) => Loc::Ns,
            (Loc::Y2, E | Wall | Broken) => Loc::Jumble,
            (Loc::Y2, W) => Loc::Windoe,
            (Loc::Y2, Plover) if !dwarf && self.toting(Obj::Emerald) => Loc::Pdrop,
            (Loc::Y2, Plover) => Loc::Proom,

            (Loc::Jumble, D) => Loc::Y2,
            (Loc::Jumble, U) => Loc::Emist,

            (Loc::Windoe, E | Y2) => Loc::Y2,
            (Loc::Windoe, Jump) => Loc::Neck,

            (Loc::Dirty, E | Crawl) => Loc::Clean,
            (Loc::Dirty, U | Hole) => Loc::Ns,
            (Loc::Dirty, W) => Loc::Dusty,
            (Loc::Dirty, Bedquilt) => Loc::Bedquilt,

            (Loc::Clean, W | Crawl) => Loc::Dirty,
            (Loc::Clean, D | Pit | Climb) => Loc::Wet,

            (Loc::Wet, Climb | U | Out) => Loc::Clean,
            (Loc::Wet, Slit | Stream | D | Upstream | Downstream) => Loc::Sayit0,

            (Loc::Dusty, E | Passage) => Loc::Dirty,
            (Loc::Dusty, D | Hole | Floor) => Loc::Complex,
            (Loc::Dusty, Bedquilt) => Loc::Bedquilt,

            (Loc::Complex, U | Climb | Room) => Loc::Dusty,
            (Loc::Complex, W | Bedquilt) => Loc::Bedquilt,
            (Loc::Complex, N | Shell) => Loc::Shell,
            (Loc::Complex, E) => Loc::Ante,

            (Loc::Shell, U | Hall) => Loc::Arch,
            (Loc::Shell, D) => Loc::Ragged,
            (Loc::Shell, S) if !dwarf && self.toting(Obj::Clam) => Loc::Sayit4,

            (Loc::Shell, S) if !dwarf && self.toting(Obj::Oyster) => Loc::Sayit5,

            (Loc::Shell, S) => Loc::Complex,

            (Loc::Arch, D | Shell | Out) => Loc::Shell,

            (Loc::Ragged, U | Shell) => Loc::Shell,
            (Loc::Ragged, D) => Loc::Sac,

            (Loc::Sac, U | Out) => Loc::Ragged,
            (Loc::Sac, Shell) => Loc::Shell,

            (Loc::Ante, U) => Loc::Complex,
            (Loc::Ante, W) => Loc::Bedquilt,
            (Loc::Ante, E) => Loc::Witt,

            (Loc::Witt, E | N | S | NE | SE | SW | NW | U | D) if pct(95) => Loc::Sayit6,
            (Loc::Witt, E) => Loc::Ante, // one chance in 20
            (Loc::Witt, W) => Loc::Sayit6,

            (Loc::Bedquilt, E) => Loc::Complex,
            (Loc::Bedquilt, W) => Loc::Cheese,
            (Loc::Bedquilt, S) if pct(80) => Loc::Sayit6,
            (Loc::Bedquilt, Slab) => Loc::Slab,
            (Loc::Bedquilt, U) if pct(50) => Loc::Abovep,
            (Loc::Bedquilt, U) => Loc::Dusty,
            (Loc::Bedquilt, N) if pct(60) => Loc::Sayit6,
            (Loc::Bedquilt, N) if pct(75) => Loc::Low,
            (Loc::Bedquilt, N) => Loc::Sjunc,
            (Loc::Bedquilt, D) if pct(80) => Loc::Sayit6,
            (Loc::Bedquilt, D) => Loc::Ante,

            (Loc::Cheese, NE) => Loc::Bedquilt,
            (Loc::Cheese, W) => Loc::E2pit,
            (Loc::Cheese, S) if pct(80) => Loc::Sayit6,
            (Loc::Cheese, Canyon) => Loc::Tall,
            (Loc::Cheese, E) => Loc::Soft,
            (Loc::Cheese, NW) if pct(50) => Loc::Sayit6,
            (Loc::Cheese, Oriental) => Loc::Oriental,

            (Loc::Soft, W | Out) => Loc::Cheese,

            (Loc::E2pit, E) => Loc::Cheese,
            (Loc::E2pit, W | Across) => Loc::W2pit,
            (Loc::E2pit, D | Pit) => Loc::Epit,

            (Loc::W2pit, E) => Loc::E2pit,
            (Loc::W2pit, W) => Loc::Slab,
            (Loc::W2pit, D) => Loc::Wpit,
            (Loc::W2pit, Hole) => Loc::Sayit7,

            (Loc::Epit, U | Out) => Loc::E2pit,

            (Loc::Wpit, U | Out) => Loc::W2pit,
            (Loc::Wpit, Climb) if self.prop[Obj::Plant] != 4 => Loc::Check,
            (Loc::Wpit, Climb) => Loc::Climb,

            (Loc::Narrow, D | Climb) => Loc::Wpit,
            (Loc::Narrow, Jump) => Loc::Neck,
            (Loc::Narrow, W | Giant) => Loc::Giant,

            (Loc::Giant, S) => Loc::Narrow,
            (Loc::Giant, E) => Loc::Block,
            (Loc::Giant, N) => Loc::Immense,

            (Loc::Block, S | Giant | Out) => Loc::Giant,

            (Loc::Immense, S | Giant | Passage) => Loc::Giant,
            (Loc::Immense, N | Enter | Cavern) if self.prop[Obj::Door] != 0 => Loc::Falls,

            (Loc::Falls, S | Out) => Loc::Immense,
            (Loc::Falls, Giant) => Loc::Giant,
            (Loc::Falls, W) => Loc::Steep,

            (Loc::Steep, N | Cavern | Passage) => Loc::Falls,
            (Loc::Steep, D | Climb) => Loc::Low,

            (Loc::Abovep, N) => Loc::Sjunc,
            (Loc::Abovep, D | Passage) => Loc::Bedquilt,
            (Loc::Abovep, S) => Loc::Tite,

            (Loc::Sjunc, SE) => Loc::Bedquilt,
            (Loc::Sjunc, S) => Loc::Abovep,
            (Loc::Sjunc, N) => Loc::Window,

            (Loc::Tite, N) => Loc::Abovep,
            (Loc::Tite, D | Jump | Climb) if pct(40) => Loc::Like6,
            (Loc::Tite, D | Jump | Climb) if pct(50) => Loc::Like9,
            (Loc::Tite, D | Jump | Climb) => Loc::Like4,

            (Loc::Low, Bedquilt) => Loc::Bedquilt,
            (Loc::Low, SW) => Loc::Scorr,
            (Loc::Low, N) => Loc::Crawl,
            (Loc::Low, SE | Oriental) => Loc::Oriental,

            (Loc::Crawl, S | Crawl | Out) => Loc::Low,

            (Loc::Window, W) => Loc::Sjunc,
            (Loc::Window, Jump) => Loc::Neck,

            (Loc::Oriental, SE) => Loc::Cheese,
            (Loc::Oriental, W | Crawl) => Loc::Low,
            (Loc::Oriental, U | N | Cavern) => Loc::Misty,

            (Loc::Misty, S | Oriental) => Loc::Oriental,
            (Loc::Misty, W) => Loc::Alcove,

            (Loc::Alcove, NW | Cavern) => Loc::Misty,
            (Loc::Alcove, E | Passage) => Loc::Ppass,
            (Loc::Alcove, E) => Loc::Proom, // never performed, but seen by ‘go back’

            (Loc::Proom, W | Passage | Out) => Loc::Ppass,
            (Loc::Proom, W) => Loc::Alcove, // never performed, but seen by ‘go back’
            (Loc::Proom, Plover) if !dwarf && self.toting(Obj::Emerald) => Loc::Pdrop,
            (Loc::Proom, Plover) => Loc::Y2,
            (Loc::Proom, NE | Dark) => Loc::Droom,

            (Loc::Droom, S | Plover | Out) => Loc::Proom,

            (Loc::Slab, S) => Loc::W2pit,
            (Loc::Slab, U | Climb) => Loc::Abover,
            (Loc::Slab, N) => Loc::Bedquilt,

            (Loc::Abover, D | Slab) => Loc::Slab,
            (Loc::Abover, S) if self.prop[Obj::Dragon] != 0 => Loc::Scan2,
            (Loc::Abover, S) => Loc::Scan1,
            (Loc::Abover, N) => Loc::Mirror,
            (Loc::Abover, Reservoir) => Loc::Res,

            (Loc::Mirror, S) => Loc::Abover,
            (Loc::Mirror, N | Reservoir) => Loc::Res,

            (Loc::Res, S | Out) => Loc::Mirror,

            (Loc::Scan1, N | Out) => Loc::Abover,

            (Loc::Immense, N | Enter | Cavern) => Loc::Sayit8,
            (Loc::Scan1, E | Forward) => Loc::Sayit9,

            (Loc::Scan2, N) => Loc::Abover,
            (Loc::Scan2, E) => Loc::Secret,

            (Loc::Scan3, E | Out) => Loc::Secret,
            (Loc::Scan3, N | Forward) => Loc::Sayit9,

            (Loc::Secret, E) => Loc::Hmk,
            (Loc::Secret, W) if self.prop[Obj::Dragon] != 0 => Loc::Scan2,
            (Loc::Secret, W) => Loc::Scan3,
            (Loc::Secret, D) => Loc::Wide,

            (Loc::Wide, S) => Loc::Tight,
            (Loc::Wide, N) => Loc::Tall,

            (Loc::Tight, N) => Loc::Wide,

            (Loc::Tall, E) => Loc::Wide,
            (Loc::Tall, W) => Loc::Boulders,
            (Loc::Tall, N | Crawl) => Loc::Cheese,

            (Loc::Boulders, S) => Loc::Tall,

            (Loc::Scorr, D) => Loc::Low,
            (Loc::Scorr, U) => Loc::Swside,

            (Loc::Swside, SW) => Loc::Scorr,
            (Loc::Swside, Over | Across | Cross | NE) if self.is_here(Obj::Troll) => Loc::Sayit10,
            (Loc::Swside, Over) if self.prop[Obj::Bridge] != 0 => Loc::Sayit11,
            (Loc::Swside, Over) => Loc::Troll,
            (Loc::Swside, Jump) if self.prop[Obj::Bridge] != 0 => Loc::Lose,
            (Loc::Swside, Jump) => Loc::Sayit2,

            (Loc::Dead0, S | Out) => Loc::Cross,
            (Loc::Dead1, W | Out) => Loc::Like11,
            (Loc::Dead2, SE) => Loc::Like13,
            (Loc::Dead3, W | Out) => Loc::Like4,
            (Loc::Dead4, E | Out) => Loc::Like4,
            (Loc::Dead5, U | Out) => Loc::Like3,
            (Loc::Dead6, W | Out) => Loc::Like9,
            (Loc::Dead7, U | Out) => Loc::Like10,
            (Loc::Dead8, E | Out) => Loc::Brink,
            (Loc::Dead9, S | Out) => Loc::Like3,
            (Loc::Dead10, E | Out) => Loc::Like12,
            (Loc::Dead11, U | Out) => Loc::Like8,

            (Loc::Neside, NE) => Loc::Corr,
            (Loc::Neside, Over | Across | Cross | SW) if self.is_here(Obj::Troll) => Loc::Sayit10,
            //(Loc::Neside, Over) if self.prop[Obj::Troll] != 0 => Loc::Sayit11,
            (Loc::Neside, Over) => Loc::Troll,
            (Loc::Neside, Jump) => Loc::Sayit2,
            (Loc::Neside, Fork) => Loc::Fork,
            (Loc::Neside, View) => Loc::View,
            (Loc::Neside, Barren) => Loc::Fbarr,

            (Loc::Corr, W) => Loc::Neside,
            (Loc::Corr, E | Fork) => Loc::Fork,
            (Loc::Corr, View) => Loc::View,
            (Loc::Corr, Barren) => Loc::Fbarr,

            (Loc::Fork, W) => Loc::Corr,
            (Loc::Fork, NE | L) => Loc::Warm,
            (Loc::Fork, SE | R | D) => Loc::Lime,
            (Loc::Fork, View) => Loc::View,
            (Loc::Fork, Barren) => Loc::Fbarr,

            (Loc::Warm, S | Fork) => Loc::Fork,
            (Loc::Warm, N | View) => Loc::View,
            (Loc::Warm, E | Crawl) => Loc::Chamber,
            (Loc::View, S | Passage | Out) => Loc::Warm,
            (Loc::View, Fork) => Loc::Fork,
            (Loc::View, D | Jump) => Loc::Sayit12,

            (Loc::Chamber, W | Out | Crawl) => Loc::Warm,
            (Loc::Chamber, Fork) => Loc::Fork,
            (Loc::Chamber, View) => Loc::View,

            (Loc::Lime, N | U | Fork) => Loc::Fork,
            (Loc::Lime, S | D | Barren) => Loc::Fbarr,
            (Loc::Lime, View) => Loc::View,
            (Loc::Fbarr, W | U) => Loc::Lime,
            (Loc::Fbarr, Fork) => Loc::Fork,
            (Loc::Fbarr, E | In | Barren | Enter) => Loc::Barr,
            (Loc::Fbarr, View) => Loc::View,

            (Loc::Barr, W | Out) => Loc::Fbarr,
            (Loc::Barr, Fork) => Loc::Fork,
            (Loc::Barr, View) => Loc::View,

            (Loc::Neend, SW | Out) => Loc::Swend,

            (Loc::Swend, NE) => Loc::Neend,
            (Loc::Swend, D) => Loc::Sayit1,

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
            _ => Loc::Limbo,
        }
    } // end lookup_instructions
}

fn is_forced(loc: Loc) -> bool {
    loc >= Loc::Crack && loc <= Loc::Troll
}

const fn init_movable() -> [bool; N_OBJECTS] {
    use Obj::*;
    let l = [
        Gold, Diamonds, Silver, Jewels, Coins, Chest, Eggs, Trident, Vase, Emerald, Pyramid, Pearl,
        Spices, Batteries, Axe, Oil, Water, Bottle, Food, Knife, Mag, Oyster, Clam, Pillow, Bird,
        Rod2, Rod, Cage, Lamp, Keys,
    ];

    let mut a = [false; N_OBJECTS];
    let mut i = 0;
    while i < l.len() {
        a[l[i] as usize] = true;
        i += 1;
    }
    a
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

fn init_loc2obj_map() -> [Vec<Obj>; N_LOC] {
    const V: Vec<Obj> = Vec::<Obj>::new();
    let mut a = [V; N_LOC];
    for tup in [
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
    tally: u8, // treasures not seen yet
    lost_treasures: u8,
    hint_count: [u16; N_HINTS],
    hinted: [bool; N_HINTS],
    words: Vec<String>,
    visits: [u16; N_LOC],
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
            dtotal: 0,
            attack: 0,
            stick: 0,
            ploc: [Loc::Limbo; 19],
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
            knife_loc: Loc::Limbo,
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
            visits: [0; N_LOC],
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
        let mut n = self.l2o[Loc::Inhand].len();
        if self.toting(Obj::Cage) && self.prop[Obj::Bird] == 1 {
            n += 1;
        }
        if self.toting(Obj::Bottle) && self.prop[Obj::Bird] != 1 {
            n += 1;
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
            Obj::Bird => (self.prop[Obj::Bird] == 1, Obj::Cage),
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
    Branch,
    GetObject,
    Cycle,
    Death,
    TryMove,
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

fn quit(g: &Game) -> ! {
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
        quit(g)
    }
    if !yes(
        DEATH_WISHES[2 * g.death_count - 2],
        DEATH_WISHES[2 * g.death_count - 1],
        OK,
    ) || g.death_count == MAX_DEATHS
    {
        quit(g)
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
    Goto::Commence
}

fn pitch_dark(g: &mut Game) -> Goto {
    println!("You fell into a pit and broke every bone in your body!");
    g.oldoldloc = g.loc;
    death(g)
}

fn dwarves_upset(g: &Game) -> ! {
    println!("The resulting ruckus has awakened the dwarves. There are now several threatening little dwarves in the room with you! Most of them throw knives at you! All of them get you!");
    quit(g)
}

fn panic_at_closing_time_180(g: &mut Game) -> &str {
    if !g.panic {
        g.clock2 = 15;
        g.panic = true;
    }
    "A mysterious recorded voice groans into life and announces: \"This exit is closed. Please leave via main office.\""
}

fn pirate_not_spotted(g: &Game) -> bool {
    g.place(Obj::Message).is_empty()
}

fn too_easy(g: &Game, o: Obj) -> bool {
    o == Obj::Pyramid && matches!(g.loc, Loc::Proom | Loc::Droom)
}

fn make_pirate_track_you_172(g: &mut Game) {
    if g.loc != MAX_PIRATE_LOC && g.prop[Obj::Chest] < 0 {
        let s = if TREASURES.iter().any(|&o| !too_easy(g, o) & g.toting(o)) {
            //⟨ Take booty and hide it in the chest 173 ⟩ ≡
            //⟨ Snatch all treasures that are snatchable here 174 ⟩ ≡
            for o in TREASURES {
                if !too_easy(g, o) && g.is_movable[o] && g.is_here(o) {
                    g.drop(o, CHEST_LOC)
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
            "Out from the shadows behind you pounces a bearded pirate! \"Har, har,\" he chortles, \"I’ll just take all this booty and hide it away with me chest deep in the maze!\" He snatches your treasure and vanishes into the gloom."
        } else if g.tally == g.lost_treasures + 1
            && !TREASURES.iter().any(|&o| g.here(o))
            && pirate_not_spotted(g)
            && g.prop[Obj::Lamp] != 0
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
        printif(s);
    }
}

fn major(g: &mut Game) -> Goto {
    //⟨ Check for interference with the proposed move to newloc 153 ⟩ ≡
    if g.closing() && g.newloc < MIN_IN_CAVE && g.newloc != Loc::Limbo {
        g.newloc = g.loc;
        println!("{}", panic_at_closing_time_180(g));
    } else if g.newloc != g.loc
        && g.newloc <= MAX_PIRATE_LOC
        //&& (1..g.odloc.len()).any(|j| g.odloc[j] == g.newloc && g.dseen[j])
        && (1..g.dloc.len()).any(|j| g.dloc[j] == g.newloc && g.dseen[j])
    {
        //⟨ Stay in loc if a dwarf is blocking the way to newloc 176 ⟩ ≡
        g.newloc = g.loc;
        println!("A little dwarf with a big knife blocks your way. \u{1F479}");
    }
    g.loc = g.newloc; // hey, we actually moved you

    //⟨ Possibly move dwarves and the pirate 161 ⟩ ≡
    if g.loc <= MAX_PIRATE_LOC && g.loc != Loc::Limbo {
        match g.dflag {
            0 => {
                if g.loc >= MIN_LOWER_LOC {
                    g.dflag = 1
                }
            }
            1 => {
                if g.loc >= MIN_LOWER_LOC && pct(5) {
                    //⟨ Advance dflag to 2 162 ⟩ ≡
                    g.dflag = 2;
                    for _ in 0..2 {
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
            }
            _ => {
                //⟨ Move dwarves and the pirate 164 ⟩ ≡
                g.dtotal = 0; // initialize totals for possible battles
                g.attack = 0;
                g.stick = 0;
                for j in 0..g.dloc.len() {
                    if g.dloc[j] != Loc::Limbo {
                        let mut i = 0;
                        //⟨ Make a table of all potential exits, ploc [0] through ploc [i − 1] 166 ⟩ ≡
                        for m in MOTIONS {
                            let newloc = g.travel(g.dloc[j], m, true);
                            if newloc != Loc::Limbo
                                && newloc >= MIN_LOWER_LOC
                                && newloc != g.odloc[j]
                                && newloc != g.dloc[j]
                                && (i == 0 || newloc != g.ploc[i - 1])
                                && i < 19
                                && newloc
                                    <= if j == 0 {
                                        MAX_PIRATE_LOC
                                    } else {
                                        MAX_NORMAL_LOC
                                    }
                            {
                                g.ploc[i] = newloc;
                                i += 1;
                            }
                        }

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
                                make_pirate_track_you_172(g)
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
                    println!(" in the room with you! \u{1F479}");
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
    Goto::Cycle
}

fn cycle(g: &mut Game) -> Goto {
    //debug(g, "cycle");
    //⟨ Check if a hint applies, and give it if requested 195 ⟩ ≡
    let bc = g.here(Obj::Bird) && g.oldobj == Obj::Bird && g.toting(Obj::Rod);
    let hint_thresh = [0, 0, 4, 5, 8, 75, 25, 20]; // turns

    for j in 2..N_HINTS {
        if !g.hinted[j] {
            if (condition(g.loc) & 2u16.pow(1 + j as u32)) == 0 {
                g.hint_count[j] = 0;
            } else if g.hint_count[j] >= hint_thresh[j] {
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
    if g.knife_loc > Loc::Limbo && g.knife_loc != g.loc {
        g.knife_loc = Loc::Limbo;
    }

    if g.closed {
        if g.prop[Obj::Oyster] < 0 && g.toting(Obj::Oyster) {
            println!("{}", Obj::Oyster.note(g.prop[Obj::Oyster]));
        }
        for o in g.l2o[Loc::Inhand].iter() {
            if g.prop[*o] < 0 {
                g.prop[*o] = -1 - g.prop[*o];
            }
        }
    }

    g.words = listen(g);

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
                g.gave_up = true;
                quit(g)
            } else if g.limit <= 30 && !g.warned && g.here(Obj::Lamp) {
                let s = if g.prop[Obj::Batteries] == 1 {
                    ", and you're out of spare batteries. You'd best start wrapping this up."
                } else if g.place(Obj::Batteries).is_empty() {
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
            Act::Go.print_msg();
            return Goto::Minor;
        } else if g.words.len() > 1 {
            g.words.remove(0);
            return Goto::Parse;
        }
    }

    // water/oil used as verbs
    if g.words.len() > 1 {
        if let Word::Object(o) = Word::from_string(&g.words[1]) {
            if (g.words[0] == "water" || g.words[0] == "oil")
                && matches!(o, Obj::Plant | Obj::Door)
                && g.is_here(o)
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

    //⟨ Look at word1 and exit to the right place if it completes a command 78 ⟩ ≡
    g.w = Word::from_string(&g.words[0]);

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
                        if water_here(g.loc) || (g.here(Obj::Bottle) && g.object_in_bottle(*o)) => {
                    }
                    Obj::Oil
                        if oil_here(g.loc) || (g.here(Obj::Bottle) && g.object_in_bottle(*o)) => {}

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
            return if g.words.len() > 1 {
                Goto::Transitive
            } else {
                Goto::Intransitive
            };
        }
        Word::Action(a) => {
            g.verb = *a;
            if g.words.len() == 1 {
                return if g.obj == Obj::Nothing {
                    Goto::Intransitive
                } else {
                    Goto::Transitive
                };
            }
        }

        Word::Message(m) => {
            println!("{}", m);
            return Goto::Minor;
        }
    }

    g.words.remove(0);
    Goto::Parse
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

fn change_to(g: &mut Game, verb: Act) -> Goto {
    g.oldverb = verb;
    g.verb = verb;
    Goto::Transitive
}

fn transitive(g: &mut Game) -> Goto {
    let s=match g.verb {
        //⟨ Handle cases of transitive verbs and continue 97 ⟩ ≡
        Act::Say => {
            if g.words.len() > 1 {
                g.words.remove(0);
            }
            g.w = Word::from_string(&g.words[0]);
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
                _ => Act::Eat.msg(),
            }
        }
        Act::Wave if g.obj==Obj::Rod && g.toting(Obj::Rod) && matches!(g.loc, Loc::Efiss | Loc::Wfiss) && !g.closing() => {
            g.prop[Obj::Crystal] = 1 - g.prop[Obj::Crystal];
            Obj::Crystal.note(2 - g.prop[Obj::Crystal])
        }
        Act::Wave if !g.toting(g.obj) =>
               Act::Drop.msg(), // don't carry it 

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
            quit(g)
        }
        Act::Rub if g.obj != Obj::Lamp => Act::Toss.msg(),
        Act::Find | Act::Inventory if g.toting(g.obj) => Act::Take.msg(),
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
                dwarves_upset(g)
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
            dwarves_upset(g)
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
            Act::Eat.msg()
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
                g.verb.msg()
            } else if !matches!(g.obj, Obj::Water | Obj::Oil) {
                "You can’t pour that." 
            } else {
                g.prop[Obj::Bottle] = 1;
                if g.is_here(Obj::Plant) {
                    //⟨ Try to water the plant 108 ⟩ ≡
                    if g.obj != Obj::Water {
                        "The plant indignantly shakes the oil off its leaves and asks, \"Water?\""
                    } else {
                        println!("{}", Obj::Plant.note(g.prop[Obj::Plant]));
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
                Act::Drop.msg()
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
            "You can't carry anything more. You'll have to drop something first."
        }
        Act::Take if g.obj==Obj::Bird && !g.toting(Obj::Cage) => "You can catch the bird, but you cannot carry it.",
        Act::Take if g.obj==Obj::Bird && g.toting(Obj::Rod) => "The bird was unafraid when you entered, but as you approach it becomes disturbed and you cannot catch it.",
        Act::Take if matches!(g.obj, Obj::Water | Obj::Oil) && g.is_here(Obj::Bottle) && g.object_in_bottle(g.obj) => { g.carry(Obj::Bottle); OK}
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

            if g.obj==Obj::Bird && g.prop[Obj::Bird]==0 {
                g.prop[Obj::Bird]=1;
                g.obj=Obj::Cage;
                g.remove(Obj::Bird)
            }
            g.carry(g.obj);
            OK
        }

        Act::Drop if g.obj==Obj::Rod && !g.toting(Obj::Rod) && g.toting(Obj::Rod2) => {
            g.drop(Obj::Rod2, g.loc);
            OK
        }

        Act::Drop if matches!(g.obj,Obj::Water | Obj::Oil) && g.toting(Obj::Bottle) && g.object_in_bottle(g.obj) => {
             g.drop(Obj::Bottle, g.loc);
             OK
        }

        Act::Drop if !g.toting(g.obj) => Act::Drop.msg(),

        Act::Drop if g.obj==Obj::Coins && g.here(Obj::Pony) => {
            //⟨ Put coins in the vending machine 118 ⟩ ≡ 
            g.remove(Obj::Coins);
            g.drop(Obj::Batteries, g.loc);
            g.prop[Obj::Batteries]=0;
            Obj::Batteries.note(0)
        }

        Act::Drop if g.obj==Obj::Bird =>
            //⟨ Check special cases for dropping the bird 120 ⟩ ≡ 
            if g.here(Obj::Snake) {
                if g.closed {dwarves_upset(g)};
                g.remove(Obj::Snake);
                g.prop[Obj::Snake]=1; // used in conditional instructions 
                "The little bird attacks the green snake, and in an astounding flurry drives the snake away." 
            } else if g.is_here(Obj::Dragon) && g.prop[Obj::Dragon]==0 {
                g.remove(Obj::Bird);
                g.prop[Obj::Bird]=0;
                if g.is_at(Obj::Snake, Loc::Hmk) {
                    g.lost_treasures+=1;
                }
                "The little bird attacks the green dragon, and in an astounding flurry gets burnt to a cinder. The ashes blow away." 
            } else {
                g.prop[Obj::Bird]=1;
                g.drop(Obj::Bird, g.loc);
                OK
            }

        Act::Drop if g.obj==Obj::Vase && g.loc!=Loc::Soft => {
            //⟨ Check special cases for dropping the vase 121 ⟩ ≡ 
            g.drop(Obj::Vase, g.loc);
            if g.is_here(Obj::Pillow) {
                g.prop[Obj::Vase]=0;
            } else {
                g.is_movable[Obj::Vase] = false;
                g.prop[Obj::Vase]=2;
            };
            Obj::Vase.note(g.prop[Obj::Vase])
        }

        Act::Drop if g.obj==Obj::Bear && g.is_here(Obj::Troll) => {
            //⟨ Chase the troll away 119 ⟩ ≡ 
            g.remove(Obj::Troll);
            g.drop(Obj::Troll2, Loc::Swside);
            g.drop(Obj::Troll2, Loc::Neside);
            g.prop[Obj::Troll]=2;
            g.drop(Obj::Bear, g.loc);
            "The bear lumbers toward the troll, who lets out a startled shriek and scurries away. The bear soon gives up the pursuit and wanders back."
        }

        Act::Drop => {
            g.drop(g.obj , g.loc );
            OK
        }

        Act::Toss if g.obj==Obj::Rod && g.toting(Obj::Rod2) && !g.toting(Obj::Rod) => {
            g.obj=Obj::Rod2;
            return change_to(g,Act::Drop)
        }

        Act::Toss if g.toting(g.obj) && g.obj.treasure() && g.is_here(Obj::Troll) => {
            //⟨ Snarf a treasure for the troll 124 ⟩ ≡ 
            //g.drop(g.obj, Loc::Limbo);
            g.remove(g.obj);
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
            return change_to(g,Act::Feed)
        }

        Act::Toss if g.toting(g.obj) && g.obj!=Obj::Axe => {
            return change_to(g,Act::Drop)
        }

        Act::Toss if g.toting(g.obj) && g.obj==Obj::Axe => {
            //⟨Throw the axe at a dwarf 163⟩ ≡ 
            if let Some(j) = g.dloc.iter().skip(1).position(|l| *l == g.loc) {
                g.drop(Obj::Axe,g.loc);
                if ran(3) < 2 {
                    g.dloc[j+1] = Loc::Limbo;
                    g.dseen[j+1] = true;
                    g.dkill+=1;
                    if g.dkill ==1 {
                        "You killed a little dwarf. The body vanishes in a cloud of greasy black smoke."
                    } else {
                        "You killed a little dwarf."
                    }
                } else {
                    "You attack a little dwarf, but he dodges out of the way."
                }
            } else if g.is_here(Obj::Dragon) && g.prop[Obj::Dragon] == 0 {
                g.drop(Obj::Axe,g.loc);
                "The axe bounces harmlessly off the dragon's thick scales." 
            } else if g.is_here(Obj::Troll) {
                "The troll deftly catches the axe, examines it carefully, and tosses it back, declaring, \"Good workmanship, but it's not valuable enough.\""
            } else if g.here(Obj::Bear) && g.prop[Obj::Bear]==0 {
                //⟨Throw the axe at the bear 123⟩ ≡
                g.drop(Obj::Axe,g.loc);
                g.prop[Obj::Axe]=1;
                g.is_movable[Obj::Axe]=false;
                "The axe misses and lands near the bear where you can't get at it."
            } else {
                g.obj = Obj::Nothing;
                return change_to(g,Act::Kill)
            }
         }

         Act::Kill  => {
            if g.obj==Obj::Nothing {
                let mut k=0;
                //⟨ See if there’s a unique object to attack 126 ⟩ ≡ {
                if g.dwarf() {
                    k+=1;
                    g.obj=Obj::Dwarf;
                }
                for (o,flag) in [(Obj::Snake,true), (Obj::Troll,true),
                     (Obj::Dragon, g.prop[Obj::Dragon]==0),
                     (Obj::Bear, g.prop[Obj::Bear]==0)]  {
                    if g.here(o) && flag {
                        k+=1;
                        g.obj=o;
                    }
                }

                if k==0 { // no enemies present
                     if g.here(Obj::Bird) && g.oldverb!=Act::Toss {
                         k+=1;
                         g.obj=Obj::Bird;
                     }
                     // no harm done to call the oyster a clam in this case  
                     if g.here(Obj::Clam) || g.here(Obj::Oyster) {
                         k+=1;
                         g.obj=Obj::Clam;
                     }
                }
                if k > 1 {
                    return Goto::GetObject
                }
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
                Obj::Dragon if g.prop[Obj::Dragon]==0 => {
                    //⟨ Fun stuff for dragon 128 ⟩ ≡ 
                    println!("With what? Your bare hands?");
                    g.verb = Act::Abstain;
                    g.obj = Obj::Nothing;
                    g.words=listen(g);
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
                    Obj::Dragon.note(1)
                }
                Obj::Dragon if g.prop[Obj::Dragon]!=0 =>
                   "For crying out loud, the poor thing is already dead!",
                Obj::Clam | Obj::Oyster =>
                   "The shell is very strong and impervious to attack.",
                Obj::Snake =>
                   "Attacking the snake both doesn't work and is very dangerous.",
                Obj::Dwarf if g.closed => dwarves_upset(g),
                Obj::Dwarf => "With what? Your bare hands?",
                Obj::Troll => "Trolls are close relatives with the rocks and have skin as tough as a rhinoceros hide. The troll fends off your blows effortlessly.",
                Obj::Bear if g.prop[Obj::Bear]==0 =>
                    "With what? Your bare hands? Against HIS bear hands?",
                Obj::Bear if g.prop[Obj::Bear]==3 =>
                   "For crying out loud, the poor thing is already dead!",
                Obj::Bear =>
                   "The bear is confused; he only wants to be your friend.",
                _ => Act::Kill.msg(),
            }
        }
        Act::Feed if g.obj==Obj::Bird  =>
            "It's not hungry (it's merely pinin' for the fjords). Besides, you have no bird seed.",
        Act::Feed if g.obj==Obj::Troll=>
              "Gluttony is not one of the troll's vices. Avarice, however, is.",
        Act::Feed if g.obj==Obj::Dragon && g.prop[Obj::Dragon]==0=>
         "There's nothing here it wants to eat (except perhaps you).",
        Act::Feed if g.obj==Obj::Dragon && g.prop[Obj::Dragon]!=0=>
         Act::Eat.msg(),
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
            Act::Eat.msg()
        }
        Act::Feed if g.obj==Obj::Dwarf && g.here(Obj::Food) => {
            g.dflag+=1;
            "You fool, dwarves eat only coal! Now you've made him REALLY mad!" 
        }
        Act::Feed => Act::Calm.msg(),
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
            g.drop(Obj::Pearl,Loc::Sac);
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
            "There is nothing here to which the chain can be locked.",
        Act::Close if g.obj==Obj::Chain && g.prop[Obj::Chain]!=0 =>
            "It was already locked.",
        Act::Close if g.obj==Obj::Chain => {
            g.prop[Obj::Chain]=2;
            g.is_movable[Obj::Chain] = false;
            if g.toting(Obj::Chain) {
                g.drop(Obj::Chain, g.loc);
            }
            "The chain is now locked."
        }
        Act::Open | Act::Close if g.obj==Obj::Grate && g.closing() =>
            panic_at_closing_time_180(g),
        Act::Open | Act::Close if g.obj==Obj::Grate => {
            let k=g.prop[Obj::Grate];
            g.prop[Obj::Grate]=if g.verb==Act::Open {1} else {0};
            match k+2*g.prop[Obj::Grate]  {
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
            Obj::Door if g.prop[Obj::Door]!=0 => Act::Relax.msg(),
            Obj::Door  => "The door is extremely rusty and refuses to open.", 
            _ => g.verb.msg(),
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
        _ => g.verb.msg(),
    };
    printif(s);

    Goto::Minor
}

fn intransitive(g: &mut Game) -> Goto {
    let s = match g.verb {
        Act::Go | Act::Relax => g.verb.msg(),
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
                "There is nothing here with a lock!"
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
            return if g.obj != Obj::Nothing {
                Goto::Transitive
            } else {
                Goto::GetObject
            };
        }
        Act::Inventory => {
            if g.l2o[Loc::Inhand].is_empty() {
                "You're not carrying anything."
            } else {
                println!("You are currently holding the following:");
                for o in g.l2o[Loc::Inhand].iter() {
                    match *o {
                        Obj::Bear => (),
                        Obj::Cage => {
                            println!(" {}", o.name());
                            if g.prop[Obj::Bird] == 1 {
                                println!(" {}", Obj::Bird.name())
                            }
                        }
                        Obj::Bottle => {
                            println!(" {}", o.name());
                            match g.prop[Obj::Bottle] {
                                0 => println!(" {}", Obj::Water.name()),
                                1 => (),
                                2 => println!(" {}", Obj::Oil.name()),
                                _ => panic!("Not valid"),
                            }
                        }
                        _ => println!(" {}", o.name()),
                    }
                }
                if g.toting(Obj::Bear) {
                    "You are being followed by a very large, tame bear."
                } else {
                    ""
                }
            }
        }
        Act::Brief => {
            g.interval = 10000;
            g.look_count = 3;
            "Okay, from now on I'll only describe a place in full the first time you come to it. To get the full description, say \"LOOK\"."
        }
        Act::Score => {
            println!(
                "If you were to quit now, you would score {} out of a possible {MAX_SCORE}.",
                score(g) - 4
            );
            g.gave_up = yes("Do you indeed wish to quit now?", OK, OK);
            if g.gave_up {
                quit(g);
            }
            ""
        }
        Act::Quit => {
            g.gave_up = yes("Do you really wish to quit now?", OK, OK);
            if g.gave_up {
                quit(g);
            }
            ""
        }
        Act::Feefie => {
            if let Some(k) = ["fee", "fie", "foe", "foo", "fum"]
                .iter()
                .position(|&s| s == g.words[0])
            {
                let k = k as i32;
                if g.foobar == -k {
                    //⟨ Proceed foobarically 139 ⟩ ≡
                    g.foobar = k + 1;
                    if g.foobar != 4 {
                        Act::Relax.print_msg();
                        return Goto::Minor;
                    }
                    g.foobar = 0;
                    if g.is_at(Obj::Eggs, Loc::Giant)
                        || (g.toting(Obj::Eggs) && g.loc == Loc::Giant)
                    {
                        Act::Wave.print_msg(); // nada sucede
                        return Goto::Minor;
                    }
                    if g.place(Obj::Eggs).is_empty()
                        && g.place(Obj::Troll).is_empty()
                        && g.prop[Obj::Troll] == 0
                    {
                        g.prop[Obj::Troll] = 1;
                    }
                    println!(
                        "{}",
                        match (g.loc == Loc::Giant, g.here(Obj::Eggs)) {
                            (true, _) => Obj::Eggs.note(0),
                            (_, true) => Obj::Eggs.note(1),
                            (_, false) => Obj::Eggs.note(2),
                        }
                    );
                    g.drop(Obj::Eggs, Loc::Giant);
                    return Goto::Minor;
                }
            };
            if g.foobar == 0 {
                Act::Wave.msg() // nada sucede
            } else {
                "What's the matter, can't you read? Now you'd best start over."
            }
        }
        _ => {
            println!("{} what?", g.words[0]);
            return Goto::Cycle;
        }
    };
    printif(s);
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
            return pitch_dark(g);
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
    println!("\n{s}");

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
                    if matches!(o, Obj::Rug | Obj::Chain) {
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
                println!(
                    "{}",
                    if o == Obj::Treads && g.loc == Loc::Emist {
                        o.note(g.prop[o] + 1)
                    } else {
                        o.note(g.prop[o])
                    }
                )
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
            } else if MOTIONS.iter().any(|&m| g.travel(g.loc, m, false)==l) {
                g.newloc=l;
                return Goto::GoForIt
            } else {
                "You can't get there from here." 
            }
       }
    _ => {
           g.newloc=g.travel(g.loc, g.mot, false);
           return Goto::GoForIt
         }
    };
    printif(s);
    Goto::Major
}

fn go_for_it(g: &mut Game) -> Goto {
    g.oldoldloc = g.oldloc;
    g.oldloc = g.loc;

    let s = match g.newloc {
        Loc::Limbo => {
            //⟨ Report on inapplicable motion and continue 148 ⟩ ≡
            g.newloc = g.loc;
            match g.mot {
            Mot::Crawl => "Which way?",
            Mot::Xyzzy | Mot::Plugh => Act::Wave.msg(),
            Mot::In | Mot::Out =>
                  "I don't know in from out here. Use compass points or name something in the general direction you want to go.",
            Mot::Forward | Mot::L | Mot::R =>
                   "I am unsure how you are facing. Use compass points or nearby objects.",
            _ if g.mot<=Mot::Forward => "There is no way to go in that direction.",
            _ if matches!(g.verb, Act::Find | Act::Inventory) => Act::Find.msg(),
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
            let q = g.newloc as usize - Loc::Sayit0 as usize;
            g.newloc = g.loc;
            SAYIT[q]
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
            Obj::Troll.note(1)
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
    printif(s);

    Goto::Major
}

fn main() {
    let mut g = Game::new();

    g.offer(0);
    g.limit = if g.hinted[0] { 1000 } else { 330 };

    //shortest_route(&g, Loc::House, Loc::Bedquilt);
    //g.newloc=Loc::Pony;
    //g.drop(Obj::Lamp, Loc::Inhand);
    //g.prop[Obj::Lamp]=1;

    let mut state = Goto::Major;
    loop {
        state = match state {
            Goto::Branch => branch(&mut g),
            Goto::GetObject => get_object(&g),
            Goto::TryMove => try_move(&mut g),
            Goto::CantSeeIt => cant_see_it(&mut g),
            Goto::Major => major(&mut g),
            Goto::Minor => minor(&mut g),
            Goto::Cycle => cycle(&mut g),
            Goto::Death => death(&mut g),
            Goto::Commence => commence(&mut g),
            Goto::Transitive => transitive(&mut g),
            Goto::Intransitive => intransitive(&mut g),
            Goto::Parse => parse(&mut g),
            Goto::PreParse => pre_parse(&mut g),
            Goto::GoForIt => go_for_it(&mut g),
        }
    }
}
