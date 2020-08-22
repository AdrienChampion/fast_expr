//! Collection-like data handling.

prelude! {}

#[derive(Debug, Clone)]
struct CollSpec {
    pub typ: StaticTypPath,
    pub ref_iter: StaticTypPath,
    pub own_iter: StaticTypPath,
}
impl CollSpec {
    pub fn typ_id(&self) -> StaticTypId {
        self.typ.id
    }
    pub fn typ(&self) -> StaticTypPath {
        self.typ
    }
    pub fn iter(&self, is_own: IsOwn) -> StaticTypPath {
        if is_own {
            self.own_iter
        } else {
            self.ref_iter
        }
    }
}

macro_rules! coll_spec {
    (
        $($field:ident : ($pref:expr, $id:expr)),* $(,)?
    ) => {
        CollSpec {
            $($field: static_typ_path! {
                pref: $pref,
                id: $id,
            }),*
        }
    };
}

static COLL_PATH: [&'static str; 2] = ["std", "collection"];
static BTREE_SET_PATH: [&'static str; 3] = [COLL_PATH[0], COLL_PATH[1], "btree_set"];
static HASH_SET_PATH: [&'static str; 3] = [COLL_PATH[0], COLL_PATH[1], "hash_set"];
static VEC_PATH: [&'static str; 2] = ["std", "vec"];
static SLICE_PATH: [&'static str; 2] = ["std", "slice"];

static VEC: CollSpec = coll_spec! {
    typ: (&VEC_PATH, "Vec"),
    ref_iter: (&SLICE_PATH, "Iter"),
    own_iter: (&VEC_PATH, "Iter"),
};
static HASH_SET: CollSpec = coll_spec! {
    typ: (&COLL_PATH, "HashSet"),
    ref_iter: (&HASH_SET_PATH, "Iter"),
    own_iter: (&COLL_PATH, "IntoIter"),
};
static BTREE_SET: CollSpec = coll_spec! {
    typ: (&COLL_PATH, "BTreeSet"),
    ref_iter: (&BTREE_SET_PATH, "Iter"),
    own_iter: (&BTREE_SET_PATH, "IntoIter"),
};

#[derive(Debug, Clone, Copy)]
pub enum Coll {
    Vec,
    HashSet,
    BTreeSet,
}
impl Coll {
    pub const PREF: &'static str = "coll";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == VEC.typ_id() {
            Ok(Self::Vec)
        } else if id == HASH_SET.typ_id() {
            Ok(Self::HashSet)
        } else if id == BTREE_SET.typ_id() {
            Ok(Self::BTreeSet)
        } else {
            bail!(on(id, "unknown collection type"))
        }
    }

    fn spec(self) -> &'static CollSpec {
        match self {
            Self::Vec => &VEC,
            Self::HashSet => &HASH_SET,
            Self::BTreeSet => &BTREE_SET,
        }
    }
    pub fn wrap(self, coll_span: rust::Span, inner: rust::Typ) -> rust::Typ {
        self.spec()
            .typ()
            .to_typ(coll_span, Some(vec![rust::GenericArg::Type(inner)]))
    }
}
