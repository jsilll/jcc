pub mod cfg;
pub mod dom;
pub mod order;

use crate::ir::Block;

use jcc_entity::EntityList;

type BlockList = EntityList<Block>;
