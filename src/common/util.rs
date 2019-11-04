use std::{any::{Any, TypeId}, collections::HashMap, hash::Hash};

pub struct TypeMap<V: Hash> {
    mapping: HashMap<TypeId, V>
}

impl<V: Hash> Default for TypeMap<V> {
    fn default() -> Self {
        TypeMap {
            mapping: Default::default()
        }
    }
}

impl<V: Hash> TypeMap<V> {
    pub fn new() -> Self {
        TypeMap::default()
    }

    pub fn insert<K: 'static>(&mut self, value: V) -> Option<V> {
        let k = TypeId::of::<K>();
        self.mapping.insert(k, value)
    }

    pub fn get<K: 'static>(&self) -> Option<&V> {
        let k = TypeId::of::<K>();
        self.mapping.get(&k)
    }
}
