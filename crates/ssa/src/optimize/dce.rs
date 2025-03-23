use std::collections::{HashMap, HashSet};

use crate::{InstKind, InstRef, Program};

pub fn eliminate_dead_code(p: &mut Program) {
    let alive = mark_live_instructions(p);
    println!("alive: {:?}", alive);
    for b in p.block_iter() {
        p.block_mut(b)
            .insts
            .retain(|inst_id| alive.contains(inst_id));
    }
    // TODO: After mark the arena slots as empty and add them to the free list
}

fn mark_live_instructions(p: &Program) -> HashSet<InstRef> {
    let mut worklist = Vec::new();
    let mut alive = HashSet::new();

    for block in p.block_iter().map(|b| p.block(b)) {
        for i in block.insts.iter() {
            if p.inst(*i).has_side_effects() {
                alive.insert(*i);
                worklist.push(*i);
            }
        }
    }

    let mut args = Vec::new();
    while let Some(i) = worklist.pop() {
        p.inst(i).get_args(&mut args);
        for arg in &args {
            if !alive.contains(&arg) {
                alive.insert(*arg);
                worklist.push(*arg);
            }
        }
        args.clear();
    }

    alive
}

pub fn remove_redundant_identity(p: &mut Program) {
    let mut identity_map: HashMap<InstRef, InstRef> = HashMap::new();
    for b in p.block_iter() {
        for i in &p.block(b).insts {
            if let InstKind::Identity(val) = &p.inst(*i).kind {
                identity_map.insert(*i, *val);
            }
        }
    }

    let mut replacement_map: HashMap<InstRef, InstRef> = HashMap::new();
    for (&i, &arg) in &identity_map {
        let mut current = arg;
        while let Some(&next) = identity_map.get(&current) {
            current = next;
        }
        replacement_map.insert(i, current);
    }

    for b in p.block_iter() {
        for idx in 0..p.block(b).insts.len() {
            let inst = p.inst_mut(p.block(b).insts[idx]);
            if let InstKind::Identity { .. } = inst.kind {
                inst.replace_args(&replacement_map);
            }
        }
    }

    eliminate_dead_code(p);
}
