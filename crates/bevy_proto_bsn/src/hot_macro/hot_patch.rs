use std::{
    any::TypeId,
    sync::{Arc, Mutex},
};

use bevy::{
    ecs::{bundle::Bundle, hierarchy::Children, world::World},
    log::{debug, error, warn},
    platform_support::{
        collections::{HashMap, HashSet},
        hash::NoOpHash,
    },
    reflect::{PartialReflect, TypeRegistry},
    utils::TypeIdMap,
};
use bevy_proto_bsn_ast::{syn, BsnAstEntity};

use crate::{
    Bsn, BsnComponent, BsnEntity, BsnReflector, DynamicPatch, DynamicScene, EntityPatch, Patch,
    Scene,
};

use super::hash;

/// The bundle/component part of a hot patch.
#[derive(Debug, Clone)]
struct HotBundlePatch {
    hash: u64,
    component_patches: Arc<TypeIdMap<Box<dyn PartialReflect>>>,
    removed_components: Arc<HashSet<TypeId, NoOpHash>>,
}

impl HotBundlePatch {
    fn new(components: &[BsnComponent], reflector: &BsnReflector) -> Self {
        let bundle_hash = hash(components);

        Self {
            hash: bundle_hash,
            component_patches: Arc::new(
                components
                    .iter()
                    .filter_map(|bsn_component| {
                        reflector
                            .reflect_component_patch(bsn_component)
                            .map(|reflected| (reflected.type_id, reflected.props.instance))
                            .map_err(|err| {
                                warn!("Failed to reflect component, skipping hot reload for this component: {}", err);
                            })
                            .ok()
                    })
                    .collect(),
            ),
            removed_components: Arc::new(HashSet::default()),
        }
    }

    #[allow(dead_code)]
    fn diff(
        components: &[BsnComponent],
        original_bundle: &OriginalBundle,
        reflector: &BsnReflector,
    ) -> Self {
        let bundle_hash = hash(components);

        // Add patches for new or changed components
        let component_count = components.len();
        let mut component_patches = TypeIdMap::with_capacity_and_hasher(component_count, NoOpHash);
        let mut keep_components = HashSet::with_capacity_and_hasher(component_count, NoOpHash);
        for bsn_component in components.iter() {
            // If there is a matching component in original_bundle (by hash), keep it.
            let component_hash = hash(bsn_component);
            if let Some(type_id) = original_bundle.get_by_hash(component_hash) {
                keep_components.insert(type_id);
                continue;
            }

            // Otherwise, try to reflect it and add it to the patch.
            match reflector.reflect_component_patch(bsn_component) {
                Ok(reflected) => {
                    for field_err in reflected.skipped_fields.iter() {
                        // TODO: Detect _removed_ fields and ensure they are overwritten with Default::default(), or somehow edit the og. patch.
                        // Maybe this is why we want to do more type-aware stuff during the hook?
                        warn!("Skipping non-reflectable field: {}", field_err);
                    }

                    component_patches.insert(reflected.type_id, reflected.props.instance);
                    keep_components.insert(reflected.type_id);
                }
                Err(err) => {
                    warn!("Failed to reflect changed component, skipping hot reload for this component: {}", err);
                }
            }
        }

        // Detect removed components
        let mut removed_components = HashSet::with_hasher(NoOpHash);
        for type_id in original_bundle.iter() {
            if !keep_components.contains(type_id) {
                debug!("Component removed: {:?}", type_id);
                removed_components.insert(*type_id);
            }
        }

        Self {
            hash: bundle_hash,
            component_patches: Arc::new(component_patches),
            removed_components: Arc::new(removed_components),
        }
    }
}

impl DynamicPatch for HotBundlePatch {
    fn dynamic_patch(self, scene: &mut DynamicScene) {
        for (type_id, patch) in self.component_patches.iter() {
            let patch = patch.clone_value();
            scene.patch_reflected(*type_id, move |props: &mut dyn PartialReflect| {
                props.apply(patch.as_ref());
            });
        }
        for type_id in self.removed_components.iter() {
            scene.remove_component(*type_id);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum HotItem {
    OriginalEntity(usize),
    // OriginalSpread(usize),
    NewEntity(HotPatch),
}

/// A patch to apply hot-reloaded bsn! changes to a [`DynamicScene`].
#[derive(Default, Debug, Clone)]
pub struct HotPatch {
    active: bool,
    has_bundle_changes: bool,
    original_bundle: Option<Arc<OriginalBundle>>,
    bundle_patch: Option<HotBundlePatch>,
    related: HotRelated,
    replace_with: Option<Arc<HotPatch>>,
}

pub(crate) type HotDeferred = Arc<Mutex<HashMap<usize, DynamicScene>>>;
pub(crate) type HotRelated = TypeIdMap<Arc<Vec<HotItem>>>;

impl HotPatch {
    fn needs_original_bundle(&self) -> bool {
        self.active && self.has_bundle_changes && self.original_bundle.is_none()
    }

    /// Overrides `dynamic_patch` for an [`EntityPatch`] with a [`HotPatch`].
    #[inline]
    pub fn dynamic_patch_override<I, P, C>(
        self,
        entity_patch: EntityPatch<I, P, C>,
        dynamic_scene: &mut DynamicScene,
    ) where
        I: Scene,
        P: Patch + DynamicPatch,
        C: Scene,
    {
        let EntityPatch {
            inherit,
            patch,
            children,
            hot_id,
            ..
        } = entity_patch;

        let is_root = hot_id.entity_index == 0;
        let mut previous_deferred = None;
        if is_root {
            // Store deferred items above this invocation (might exist if this is an inherited scene)
            previous_deferred = dynamic_scene.hot_deferred.take();
            // Set up the deferred map for this invocation
            dynamic_scene.hot_deferred = Some(Default::default());
        }

        // Apply the inherited patches
        // TODO: Support hot-reloading changes to inheritance list
        inherit.dynamic_patch(dynamic_scene);

        // Apply this patch itself
        patch.dynamic_patch(dynamic_scene);
        dynamic_scene.hot_id = Some(hot_id);

        // TODO: Patch components
        // if let Some(bundle_patch) = self.bundle_patch {
        //   if self.active {}
        // }
        // Apply component changes
        // if let Some(bundle_patch) = self.bundle_patch {
        //     for (type_id, patch) in bundle_patch.component_patches.iter() {
        //         let patch = patch.clone();
        //         dynamic_scene.patch_reflected(*type_id, move |props: &mut dyn PartialReflect| {
        //             props.apply(patch.as_ref());
        //         });
        //     }
        //     for type_id in bundle_patch.removed_components.iter() {
        //         // TODO: This should not remove inherited components
        //         dynamic_scene.remove_component(*type_id);
        //     }
        // }

        // Recursively create/patch dynamic scenes for (original) children
        let children_has_changed = self.related.get(&TypeId::of::<Children>()).is_some();
        let hot_deferred_a = dynamic_scene.hot_deferred.clone();
        let mut hot_deferred_b = dynamic_scene.hot_deferred.clone();
        children.dynamic_patch_discrete(
            &mut || {
                let mut child_scene = DynamicScene::default();
                child_scene.hot_deferred = hot_deferred_a.clone();
                child_scene
            },
            &mut |child_scene| {
                if children_has_changed {
                    // Changed children are deferred, to be arranged later
                    hot_deferred_b
                        .as_mut()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .insert(child_scene.hot_id.unwrap().entity_index, child_scene);
                } else {
                    // Unchanged children can be directly pushed to the dynamic scene
                    dynamic_scene.push_child(child_scene);
                }
            },
        );

        // Store the new children on the dynamic_scene if changed
        if children_has_changed {
            dynamic_scene.hot_related = Some(self.related);
        }

        if is_root {
            // Make a final recursive pass over the dynamic scenes to add the deferred children in their new spots
            let hot_deferred_arc = dynamic_scene.hot_deferred.take().unwrap();
            let mut hot_deferred = hot_deferred_arc.lock().unwrap();
            Self::arrange_children(&mut hot_deferred, dynamic_scene);

            // Restore the deferred for the above invocation
            dynamic_scene.hot_deferred = previous_deferred;
        }
    }

    fn arrange_children(
        hot_deferred: &mut HashMap<usize, DynamicScene>,
        dynamic_scene: &mut DynamicScene,
    ) {
        // Push any deferred children to the dynamic scene
        if let Some(new_children) = dynamic_scene
            .hot_related
            .as_ref()
            .and_then(|hot_related| hot_related.get(&TypeId::of::<Children>()))
        {
            for hot_item in new_children.clone().iter() {
                match hot_item {
                    HotItem::OriginalEntity(original_index) => {
                        let deferred = hot_deferred
                            .remove(original_index)
                            .or_else(|| {
                                error!("deferred not found: {}", original_index);
                                None
                            })
                            .unwrap();
                        dynamic_scene.push_child(deferred);
                    }
                    HotItem::NewEntity(hot_patch) => {
                        let mut child_scene = DynamicScene::default();
                        if let Some(bundle_patch) = hot_patch.bundle_patch.clone() {
                            bundle_patch.dynamic_patch(&mut child_scene);
                        }
                        child_scene.hot_related = Some(hot_patch.related.clone());
                        dynamic_scene.push_child(child_scene);
                    }
                }
            }
        }

        // Recurse into children
        for child in dynamic_scene.children_mut().iter_mut() {
            Self::arrange_children(hot_deferred, child);
        }
    }
}

/// Holds information about a bsn!-macro invocation for hot-reloading.
#[derive(Debug)]
pub struct BsnInvocation {
    original_hash: u64,
    current_hash: u64,
    original_bsn: OriginalBsn,
    //original_bundles: HashMap<usize, OriginalBundle>,
    /// Hot patches - by original entity index
    hot_patches: HashMap<usize, HotPatch>,
    burned_original_indices: HashSet<usize>,
}

impl BsnInvocation {
    /// Returns true if this invocation has any reloaded changes from the original compiled macro.
    pub fn has_changes(&self) -> bool {
        self.original_hash != self.current_hash
    }

    /// Tries to parse and reflect a [`BsnInvocation`] from an original bsn! macro invocation.
    pub fn try_from_original(macro_ast: &syn::Macro) -> Result<Self, syn::Error> {
        let macro_hash = hash(macro_ast);
        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = OriginalBsn::from(&bsn_ast);

        Ok(Self {
            original_hash: macro_hash,
            current_hash: macro_hash,
            //original_bundles: Default::default(),
            original_bsn: bsn,
            hot_patches: Default::default(),
            burned_original_indices: Default::default(),
        })
    }

    /// Update this invocation with the changes from a reloaded macro invocation.
    pub fn reload_invocation(
        &mut self,
        macro_ast: &syn::Macro,
        registry: &TypeRegistry,
    ) -> Result<(), syn::Error> {
        let macro_hash = hash(macro_ast);
        if macro_hash == self.current_hash {
            return Ok(());
        }

        // Set all hot patches to inactive
        for hot_patch in self.hot_patches.values_mut() {
            hot_patch.active = false;
        }

        // Update the current hash and return if back to original
        self.current_hash = macro_hash;
        if self.current_hash == self.original_hash {
            return Ok(());
        }

        // Parse the new bsn
        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Bsn::from(&bsn_ast);
        let reflector = BsnReflector::new(&bsn, registry);

        // Diff and reload the new bsn recursively
        self.burned_original_indices.clear();
        let root_item = self.reload_entity(&bsn.root, &reflector);

        // Replace root patch if needed
        let root_hot_patch = self.hot_patches.entry(0).or_default();
        if let HotItem::NewEntity(replaced_root) = root_item {
            root_hot_patch.replace_with = Some(Arc::new(replaced_root));
        } else {
            root_hot_patch.replace_with = None;
        }

        Ok(())
    }

    fn reload_entity(&mut self, entity: &BsnEntity, reflector: &BsnReflector) -> HotItem {
        let entity_hash = hash(entity);

        // Check if there is an original entity with the exact same hash
        if let Some(original_entity) = self
            .original_bsn
            .first_entity_with_hash(entity_hash, &self.burned_original_indices)
        {
            // original_entity is a full match, burn this entity + descendants
            self.burned_original_indices
                .insert(original_entity.entity_index);
            self.burned_original_indices
                .extend(original_entity.descendants.iter().copied());

            return HotItem::OriginalEntity(original_entity.entity_index);
        }

        // Check for components match
        let components_hash = hash(&entity.components);
        let match_by_components = self
            .original_bsn
            .first_entity_with_components_hash(components_hash, &self.burned_original_indices)
            .map(|original_entity| original_entity.entity_index);
        if let Some(original_index) = match_by_components {
            // Found an original entity with exact matching bundle components, burn this entity, but not descendants.
            self.burned_original_indices.insert(original_index);

            // Diff the children relationship
            // TODO: Support any relationship
            let children = entity
                .children
                .iter()
                .map(|child| self.reload_entity(child, reflector))
                .collect::<Vec<_>>();
            let original_entity = self
                .original_bsn
                .get_entity_by_index(original_index)
                .expect("entity index mismatch");
            let relationship_changed = children.len() != original_entity.children.len()
                || children.iter().zip(original_entity.children.iter()).any(
                    |(related, original_index)| match related {
                        HotItem::OriginalEntity(related_index) => *related_index != *original_index,
                        HotItem::NewEntity(_) => true, // Non original entities means the relationship has changed
                    },
                );
            if relationship_changed {
                // The children relationship has changed, create or update the hot patch for this entity
                let hot_patch = self.hot_patches.entry(original_index).or_default();
                hot_patch.active = true;
                hot_patch
                    .related
                    .insert(TypeId::of::<Children>(), Arc::new(children));
            }

            return HotItem::OriginalEntity(original_index);
        }

        // TODO: Check for non-reflectable component/field match to allow patching of partially non-reflectable entities/components

        // No match found, create a new hot patch for this entity
        let mut hot_patch = HotPatch {
            active: true,
            has_bundle_changes: true,
            bundle_patch: Some(HotBundlePatch::new(&entity.components, reflector)),
            ..Default::default()
        };

        // Diff/reload children
        if !entity.children.is_empty() {
            let children = entity
                .children
                .iter()
                .map(|child| self.reload_entity(child, reflector))
                .collect();

            hot_patch
                .related
                .insert(TypeId::of::<Children>(), Arc::new(children));
        }

        HotItem::NewEntity(hot_patch)
    }

    /// Prepares a bsn!-invocation for hot patching.
    ///
    /// This should be called before calling [`crate::DynamicPatch::dynamic_patch`] to ensure the static bundle info is initialized.
    ///
    /// If there are any hot reloaded changes, this will set the hot patch to be applied during dynamic patch.
    #[inline]
    pub fn init_hot_patch<I, P, C>(
        &mut self,
        entity_patch: &mut EntityPatch<I, P, C>,
        world: &World,
    ) where
        I: Scene,
        P: Patch + DynamicPatch,
        C: Scene,
    {
        if !self.has_changes() {
            return;
        }

        let entity_index = entity_patch.hot_id.entity_index;

        if let Some(hot_patch) = self.hot_patches.get_mut(&entity_index) {
            if !hot_patch.active && hot_patch.replace_with.is_none() {
                return;
            }

            // Initialize the original bundle info using [`Bundle`] if needed.
            if hot_patch.needs_original_bundle() {
                let entity = self
                    .original_bsn
                    .get_entity_by_index(entity_index)
                    .expect("entity index mismatch");

                let Some(original_bundle) =
                    OriginalBundle::new::<P::Construct>(&entity.components, world)
                else {
                    warn!(
                    "Some changes could not be hot-reloaded due to unregistered components. Try saving the file again."
                );
                    return;
                };

                hot_patch.original_bundle = Some(Arc::new(original_bundle));
            }

            entity_patch.hot_patch = Some(hot_patch.clone());
        }
    }
}

#[derive(Debug)]
struct OriginalBundle {
    type_ids: Vec<TypeId>,
    indices_by_hash: HashMap<u64, usize, NoOpHash>,
}

impl OriginalBundle {
    fn new<B: Bundle>(original_components: &[BsnComponent], world: &World) -> Option<Self> {
        let size_hint = original_components.len();
        let mut type_ids = Vec::with_capacity(size_hint);
        let mut indices_by_hash = HashMap::with_capacity_and_hasher(size_hint, NoOpHash);

        let mut has_unregistered = false;
        let mut i = 0;
        B::get_component_ids(world.components(), &mut |component_id| {
            if has_unregistered || component_id.is_none() {
                has_unregistered = true;
                return;
            }

            let type_id = world
                .components()
                .get_info(component_id.unwrap())
                .unwrap()
                .type_id()
                .unwrap();

            let hash = hash(&original_components[i]);

            type_ids.push(type_id);
            indices_by_hash.insert(hash, i);

            i += 1;
        });

        if has_unregistered {
            return None;
        }

        Some(Self {
            type_ids,
            indices_by_hash,
        })
    }

    fn iter(&self) -> impl Iterator<Item = &TypeId> {
        self.type_ids.iter()
    }

    fn get_unchecked(&self, index: usize) -> TypeId {
        self.type_ids[index]
    }

    fn get_by_hash(&self, hash: u64) -> Option<TypeId> {
        self.indices_by_hash
            .get(&hash)
            .map(|i| self.get_unchecked(*i))
    }
}

#[derive(Debug)]
struct OriginalBsnEntity {
    hash: u64,
    entity_index: usize,
    components_hash: u64,
    // TODO:
    // skipped_components_hash: u64,
    components: Vec<BsnComponent>,
    children: Vec<usize>,
    descendants: Vec<usize>,
    // key: Option<BsnKey>,
}

/// A flattened [`Bsn`] tree.
#[derive(Debug)]
struct OriginalBsn {
    /// Flat list of all the entities in the Bsn, in order of appearance (depth first for relationship traversal).
    ///
    /// The indices in this list matches the [`EntityPatchId::entity_index`] output by the `bsn!` macro.
    entities: Vec<OriginalBsnEntity>,
    hash_to_entity_index: HashMap<u64, Vec<usize>, NoOpHash>,
    components_hash_to_entity_index: HashMap<u64, Vec<usize>, NoOpHash>,
    // /// Relationships between entities, the [`RelationshipType`] matches the type of [`bevy::ecs::relationship::Relationship`].
    // relationship_targets: Vec<Vec<(RelationshipTargetType, Vec<usize>)>>,
}

impl From<&BsnAstEntity> for OriginalBsn {
    fn from(ast: &BsnAstEntity) -> Self {
        Self::flatten(Bsn::from(ast))
    }
}

impl OriginalBsn {
    fn flatten(bsn: Bsn) -> Self {
        let mut flat_entities = Vec::new();
        let mut hash_to_entity_index = HashMap::with_hasher(NoOpHash);
        let mut components_hash_to_entity_index = HashMap::with_hasher(NoOpHash);
        let mut descendant_stack = Vec::new();

        fn recurse(
            entity: BsnEntity,
            flat_entities: &mut Vec<OriginalBsnEntity>,
            hash_to_entity_index: &mut HashMap<u64, Vec<usize>, NoOpHash>,
            components_hash_to_entity_index: &mut HashMap<u64, Vec<usize>, NoOpHash>,
            descendant_stack: &mut Vec<usize>,
        ) {
            let flat_entity = OriginalBsnEntity {
                hash: hash(&entity),
                entity_index: flat_entities.len(),
                components_hash: hash(&entity.components),
                components: entity.components,
                //children_hash: hash(&entity.children),
                children: Vec::new(),
                descendants: Vec::new(),
                //key: entity.key,
            };

            let entity_index = flat_entity.entity_index;

            hash_to_entity_index
                .entry(flat_entity.hash)
                .or_default()
                .push(entity_index);

            components_hash_to_entity_index
                .entry(flat_entity.components_hash)
                .or_default()
                .push(entity_index);

            flat_entities.push(flat_entity);

            let descendants_start = descendant_stack.len();
            for child in entity.children {
                let child_index = flat_entities.len();
                flat_entities[entity_index].children.push(child_index);
                descendant_stack.push(child_index);

                recurse(
                    child,
                    flat_entities,
                    hash_to_entity_index,
                    components_hash_to_entity_index,
                    descendant_stack,
                );
            }

            flat_entities[entity_index]
                .descendants
                .extend(descendant_stack.drain(descendants_start..));
        }

        recurse(
            bsn.root,
            &mut flat_entities,
            &mut hash_to_entity_index,
            &mut components_hash_to_entity_index,
            &mut descendant_stack,
        );

        Self {
            entities: flat_entities,
            hash_to_entity_index,
            components_hash_to_entity_index,
        }
    }

    fn get_entity_by_index(&self, index: usize) -> Option<&OriginalBsnEntity> {
        self.entities.get(index)
    }

    fn iter_indices_by_hash(&self, hash: u64) -> impl Iterator<Item = usize> + '_ {
        self.hash_to_entity_index
            .get(&hash)
            .into_iter()
            .flat_map(|v| v.iter().copied())
    }

    fn iter_indices_by_components_hash(&self, hash: u64) -> impl Iterator<Item = usize> + '_ {
        self.components_hash_to_entity_index
            .get(&hash)
            .into_iter()
            .flat_map(|v| v.iter().copied())
    }

    fn first_entity_with_hash(
        &self,
        hash: u64,
        exclude: &HashSet<usize>,
    ) -> Option<&OriginalBsnEntity> {
        self.iter_indices_by_hash(hash)
            .find(|i| !exclude.contains(i))
            .map(|i| &self.entities[i])
    }

    fn first_entity_with_components_hash(
        &self,
        hash: u64,
        exclude: &HashSet<usize>,
    ) -> Option<&OriginalBsnEntity> {
        self.iter_indices_by_components_hash(hash)
            .find(|i| !exclude.contains(i))
            .map(|i| &self.entities[i])
    }
}
