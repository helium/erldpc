use std::sync::{RwLock, RwLockWriteGuard};

type List = Vec<i8>;

#[derive(Debug)]
pub struct I8VecRes(pub RwLock<List>);

impl I8VecRes {
    // fn read(&self) -> RwLockReadGuard<'_, List> {
    //     self.0.read().unwrap()
    // }

    pub fn write(&self) -> RwLockWriteGuard<'_, List> {
        self.0.write().unwrap()
    }
}

impl From<List> for I8VecRes {
    fn from(other: List) -> Self {
        I8VecRes(RwLock::new(other))
    }
}
