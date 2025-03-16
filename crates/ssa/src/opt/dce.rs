use std::collections::{HashMap, HashSet};

use crate::{InstKind, InstRef, Program};

pub fn eliminate_dead_code(p: &mut Program) {
    // First, mark all live instructions
    let live_insts = mark_live_instructions(p);

    // Then, remove all dead instructions
    for b in p.get_blocks_iter() {
        let block = p.get_block_mut(b);
        block.insts.retain(|inst_id| live_insts.contains(inst_id));
    }

    // TODO: After mark the arena slots as empty and add them to the free list
}

fn mark_live_instructions(p: &Program) -> HashSet<InstRef> {
    let mut worklist = Vec::new();
    let mut live_insts = HashSet::new();

    // Initially mark all instructions with side effects as live
    for b in p.get_blocks_iter() {
        let block = p.get_block(b);
        for inst_id in &block.insts {
            let inst = p.get_inst(*inst_id);
            if !inst.get_effects(p).is_empty() {
                live_insts.insert(*inst_id);
                worklist.push(*inst_id);
            }
        }
    }

    // Propagate liveness backward
    while let Some(inst_id) = worklist.pop() {
        let inst = p.get_inst(inst_id);

        // Mark all operands as live
        if let Some(args) = inst.get_args() {
            for arg in args {
                if !live_insts.contains(&arg) {
                    live_insts.insert(arg);
                    worklist.push(arg);
                }
            }
        }
    }

    live_insts
}

pub fn remove_redundant_identity(p: &mut Program) {
    // Map to track identity instructions and their source
    let mut identity_map: HashMap<InstRef, InstRef> = HashMap::new();
    
    // First pass: collect all identity instructions
    for b in p.get_blocks_iter() {
        for i in &p.get_block(b).insts.clone() {
            let inst = p.get_inst(*i);
            if let InstKind::Identity(val) = &inst.kind {
                identity_map.insert(*i, *val);
            }
        }
    }
    
    // Second pass: follow chain of identities to find the ultimate source
    let mut replacement_map: HashMap<InstRef, InstRef> = HashMap::new();
    for (&inst_ref, &arg_ref) in &identity_map {
        let mut current = arg_ref;
        
        // Follow the chain of identities
        while let Some(&next) = identity_map.get(&current) {
            current = next;
        }
        
        replacement_map.insert(inst_ref, current);
    }
    
    // Third pass: replace all uses of identity instructions with their source
    for b in p.get_blocks_iter() {
        for i in &p.get_block(b).insts.clone() {
            let inst = p.get_inst_mut(*i);
            
            // Skip identity instructions themselves
            if matches!(inst.kind, InstKind::Identity { .. }) {
                continue;
            }
            
            // Replace all arguments that are identities with their source
            inst.replace_args(&replacement_map);
        }
    }
    
    eliminate_dead_code(p);
}