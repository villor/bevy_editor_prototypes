use core::{hash::Hash, panic};

use bevy::{
    ecs::{bundle::DynamicBundle, component::ComponentId},
    prelude::{Bundle, Children, Component, Entity, EntityCommands, EntityWorldMut, Name},
    utils::{
        hashbrown::{HashMap, HashSet},
        Hashed,
    },
};
use variadics_please::all_tuples_enumerated;

use crate::{Scene, *};

/// An anchor is an identifier for entities in a retained scene.
#[derive(Hash, Eq, PartialEq, Clone)]
pub enum Anchor {
    /// The entity is static and using an automatic incrementing ID.
    Auto(u64),
    /// The entity has been explicitly keyed with a [`Key`].
    Keyed(u64),
}

#[derive(Component, Eq, PartialEq, Hash, Clone)]
#[component(immutable)]
pub struct Key(pub HashedKey);

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct HashedKey(Hashed<String>);

impl From<&str> for HashedKey {
    fn from(s: &str) -> Self {
        HashedKey(Hashed::new(s.to_string()))
    }
}

impl From<String> for HashedKey {
    fn from(s: String) -> Self {
        HashedKey(Hashed::new(s))
    }
}

/// Receipts allow retaining of scenes that can be intelligently updated.
#[derive(Default, Component, Clone)]
pub struct Receipt {
    /// The components it inserted.
    components: HashSet<ComponentId>,
    /// The anchors of all the children it spawned/retained.
    anchors: HashMap<Anchor, Entity>,
}

/// A scene that can be retained.
pub trait RetainScene {
    /// Retains the scene by applying the patch to the entity,
    ///  removing components that should be removed, and spawning/updating children.
    ///
    /// Maintains receipts to allow for intelligent updates.
    fn retain_scene(self, entity: &mut EntityWorldMut) -> Result<(), ConstructError>;

    /// Retains the scene (or scenes if tuple) as children of the entity.
    fn retain_as_children(
        self,
        entity: &mut EntityWorldMut,
        current_anchors: HashMap<Anchor, Entity>,
    ) -> Result<HashMap<Anchor, Entity>, ConstructError>;

    /// Returns the optional hash of the [`Key`] used on the root entity of this scene.
    fn key_hash() -> Option<u64> {
        None
    }
}

impl RetainScene for () {
    fn retain_scene(self, _: &mut EntityWorldMut) -> Result<(), ConstructError> {
        panic!("tuples of scenes cannot be retained directly, did you mean retain_as_children?");
    }

    fn retain_as_children(
        self,
        entity: &mut EntityWorldMut,
        current_anchors: HashMap<Anchor, Entity>,
    ) -> Result<HashMap<Anchor, Entity>, ConstructError> {
        entity.world_scope(|world| {
            for orphan_id in current_anchors.into_values() {
                world.entity_mut(orphan_id).despawn();
            }
        });
        Ok(HashMap::new())
    }
}

// Tuple impls
macro_rules! impl_retain_scene {
    ($(#[$meta:meta])* $(($n:tt, $S:ident, $s:ident)),*) => {
        $(#[$meta])*
        impl<$($S: RetainScene),*> RetainScene for ($($S,)*) {
            fn retain_scene(self, _: &mut EntityWorldMut) -> Result<(), ConstructError> {
                panic!("tuples of scenes cannot be retained directly, did you mean retain_as_children?");
            }

            fn retain_as_children(
                self,
                entity: &mut EntityWorldMut,
                mut current_anchors: HashMap<Anchor, Entity>,
            ) -> Result<HashMap<Anchor, Entity>, ConstructError> {
                let children = [$({
                    // Compute the anchor for this fragment, (TODO) using it's name if supplied
                    // or an auto-incrementing counter if not.
                    let anchor = Anchor::Auto($n);

                    // Find the existing child entity based on the anchor, or spawn a
                    // new one.
                    let entity_id = current_anchors
                        .remove(&anchor)
                        .unwrap_or_else(|| entity.world_scope(|world| world.spawn_empty().id()));

                    // Store the anchor
                    (anchor, entity_id)
                }),*];

                // Clear any remaining orphans from the previous template. We do this
                // first (before deparenting) so that hooks still see the parent when
                // they run.
                entity.world_scope(|world| {
                    for (_, orphan_id) in current_anchors.drain() {
                        world.entity_mut(orphan_id).despawn();
                    }
                });

                // Position the entities as children.
                entity.remove::<Children>();
                entity.add_children(&[$(children[$n].1),*]);

                // Retain the children. It's important that this
                // happens *after* the entities are positioned as children to make hooks
                // work correctly.
                entity.world_scope(|world| {
                    $(
                        self.$n.retain_scene(&mut world.entity_mut(children[$n].1))?;
                    )*
                    Ok(()) as Result<(), ConstructError>
                })?;

                // Return the new anchors
                current_anchors.extend(children.into_iter());
                Ok(current_anchors)
            }
        }
    };
}

all_tuples_enumerated!(
    #[doc(fake_variadic)]
    impl_retain_scene,
    1,
    12,
    S,
    s
);

impl<I, P, C> RetainScene for EntityPatch<I, P, C>
where
    I: Scene,
    P: Patch + DynamicPatch,
    C: Scene,
{
    fn retain_scene(mut self, entity: &mut EntityWorldMut) -> Result<(), ConstructError> {
        if I::ENTITY_COUNT > 0 {
            // Has inherited scenes
            // Create a dynamic scene and build it up from the inherited patches and this patch
            let mut dynamic_scene = DynamicScene::default();
            self.dynamic_patch(&mut dynamic_scene);
            return dynamic_scene.retain_scene(entity);
        }

        // Static scene - no inheritance

        // Clone the receipt for the targeted entity.
        let receipt = entity
            .get::<Receipt>()
            .map(ToOwned::to_owned)
            .unwrap_or_default();

        // Collect set of component ids present in the patch
        let mut components = HashSet::new();
        entity.world_scope(|world| {
            <P::Construct as Bundle>::get_component_ids(world.components(), &mut |maybe_id| {
                if let Some(id) = maybe_id {
                    components.insert(id);
                }
            });
        });

        // Construct and insert the bundle
        let bundle = entity.construct_from_patch(&mut self.patch)?;
        entity.insert(bundle);

        // Remove the components in the previous bundle but not this one
        for component_id in receipt.components.difference(&components) {
            entity.remove_by_id(*component_id);
        }

        // Build the children
        let anchors = self.children.retain_as_children(entity, receipt.anchors)?;

        // Place the new receipt onto the entity
        entity.insert(Receipt {
            components,
            anchors,
        });

        Ok(())
    }

    fn retain_as_children(
        self,
        entity: &mut EntityWorldMut,
        current_anchors: HashMap<Anchor, Entity>,
    ) -> Result<HashMap<Anchor, Entity>, ConstructError> {
        (self,).retain_as_children(entity, current_anchors)
    }
}

impl RetainScene for DynamicScene {
    fn retain_scene(self, entity: &mut EntityWorldMut) -> Result<(), ConstructError> {
        // Clone the receipt for the targeted entity.
        let receipt = entity
            .get::<Receipt>()
            .map(ToOwned::to_owned)
            .unwrap_or_default();

        let entity_id = entity.id();
        entity.world_scope(|world| {
            // Collect set of component ids present in the patch
            let mut components = HashSet::new();
            components.extend(self.iter_component_ids(world.components()));

            // Construct and insert the bundle
            self.construct_components(&mut ConstructContext::new(entity_id, world))?;

            // Remove the components in the previous bundle but not this one
            let mut entity = world.entity_mut(entity_id);
            for component_id in receipt.components.difference(&components) {
                entity.remove_by_id(*component_id);
            }

            // Build the children
            let anchors = self
                .children
                .retain_as_children(&mut entity, receipt.anchors)?;

            // Place the new receipt onto the entity
            entity.insert(Receipt {
                components,
                anchors,
            });

            Ok(())
        })
    }

    fn retain_as_children(
        self,
        _entity: &mut EntityWorldMut,
        _current_anchors: HashMap<Anchor, Entity>,
    ) -> Result<HashMap<Anchor, Entity>, ConstructError> {
        panic!("dynamic scenes cannot be retained as tuples, did you mean retain_scene?");
    }
}

impl RetainScene for Vec<DynamicScene> {
    fn retain_scene(self, _entity: &mut EntityWorldMut) -> Result<(), ConstructError> {
        panic!("slices of DynamicScene cannot be directly retained on an entity, did you mean retain_as_children?");
    }

    fn retain_as_children(
        self,
        entity: &mut EntityWorldMut,
        mut current_anchors: HashMap<Anchor, Entity>,
    ) -> Result<HashMap<Anchor, Entity>, ConstructError> {
        let children = entity.world_scope(|world| {
            // Get or create an entity for each fragment.
            let mut i = 0;
            let children: Vec<_> = self
                .into_iter()
                .map(|fragment| {
                    // Compute the anchor for this fragment, using it's name if supplied
                    // or an auto-incrementing counter if not.
                    // TODO: Named anchors for dynamic scenes
                    let anchor = Anchor::Auto(i);
                    i += 1;

                    // Find the existing child entity based on the anchor, or spawn a
                    // new one.
                    let entity_id = current_anchors
                        .remove(&anchor)
                        .unwrap_or_else(|| world.spawn_empty().id());

                    // Store the fragment, it's anchor, and it's entity id.
                    (fragment, anchor, entity_id)
                })
                .collect();

            // Clear any remaining orphans from the previous template. We do this
            // first (before deparenting) so that hooks still see the parent when
            // they run.
            for orphan_id in current_anchors.into_values() {
                world.entity_mut(orphan_id).despawn();
            }

            children
        });

        // Position the entities as children.
        let child_entities: Vec<_> = children.iter().map(|(_, _, entity)| *entity).collect();
        entity.remove::<Children>();
        entity.add_children(&child_entities);

        // Build the children and produce the receipts. It's important that this
        // happens *after* the entities are positioned as children to make hooks
        // work correctly.
        entity.world_scope(|world| {
            children
                .into_iter()
                .map(|(fragment, anchor, entity_id)| {
                    fragment.retain_scene(&mut world.entity_mut(entity_id))?;
                    Ok((anchor, entity_id))
                })
                .collect()
        })
    }
}

/// Retain [`Scene`] extension.
pub trait RetainSceneExt {
    /// Retain the given [`Scene`].
    fn retain_scene(&mut self, scene: impl Scene) -> Result<(), ConstructError>;
}

impl RetainSceneExt for EntityWorldMut<'_> {
    /// Retain the given [`Scene`].
    fn retain_scene(&mut self, scene: impl Scene) -> Result<(), ConstructError> {
        scene.retain_scene(self)
    }
}

/// Retain [`Scene`] extension.
pub trait RetainSceneCommandExt {
    /// Retain the given [`Scene`].
    fn retain_scene(&mut self, scene: impl Scene + Send + 'static);
}

impl RetainSceneCommandExt for EntityCommands<'_> {
    /// Retain the given [`Scene`].
    fn retain_scene(&mut self, scene: impl Scene + Send + 'static) {
        self.queue(|mut entity: EntityWorldMut| {
            entity.retain_scene(scene).unwrap();
        });
    }
}
