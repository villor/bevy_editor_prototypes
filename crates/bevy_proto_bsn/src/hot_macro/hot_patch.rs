use std::{
    any::TypeId,
    sync::{Arc, Mutex},
};

use bevy::{
    ecs::{bundle::Bundle, hierarchy::Children, world::World},
    log::{debug, error, warn},
    platform_support::{
        collections::{HashMap, HashSet},
        hash::{FixedHasher, NoOpHash},
    },
    reflect::{PartialReflect, TypeRegistry},
    utils::TypeIdMap,
};
use bevy_proto_bsn_ast::{syn, BsnAstEntity};

use crate::{
    Bsn, BsnComponent, BsnEntity, BsnReflector, DynamicPatch, DynamicScene, EntityPatch, Patch,
    Scene,
};

use super::{hash, EntityPatchId};

/// The bundle/component part of a hot patch.
#[derive(Debug, Clone)]
struct HotBundlePatch {
    component_patches: Arc<TypeIdMap<Box<dyn PartialReflect>>>,
    removed_components: Arc<HashSet<TypeId, NoOpHash>>,
}

impl HotBundlePatch {
    fn new(components: &[BsnComponent], reflector: &BsnReflector) -> Self {
        Self {
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
    new_root: Option<Arc<HotPatch>>,
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

        // Apply the static patch if this entity is still active
        if self.active {
            if self.new_root.is_none() {
                // This is a non-root or a root that is still a root, apply it directly to the dynamic scene
                Self::dynamic_patch_original(hot_id, inherit, patch, self.related, dynamic_scene);
            } else {
                // This is a root that is no longer a root, patch it to a new dynamic scene and defer it
                let mut old_root = DynamicScene::default();
                Self::dynamic_patch_original(hot_id, inherit, patch, self.related, &mut old_root);
                dynamic_scene
                    .hot_deferred
                    .as_mut()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .insert(0, old_root);
            }
        }

        // Recursively create/patch dynamic scenes for (original) children, storing them in the deferred map
        let hot_deferred_a = dynamic_scene.hot_deferred.clone();
        let mut hot_deferred_b = dynamic_scene.hot_deferred.clone();
        children.dynamic_patch_discrete(
            &mut || {
                let mut child_scene = DynamicScene::default();
                child_scene.hot_deferred = hot_deferred_a.clone();
                child_scene
            },
            &mut |child_scene| {
                if let Some(hot_id) = child_scene.hot_id {
                    hot_deferred_b
                        .as_mut()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .insert(hot_id.entity_index, child_scene);
                }
            },
        );

        if is_root {
            // Patch new root if we have one
            if let Some(new_root) = self.new_root {
                if let Some(bundle_patch) = new_root.bundle_patch.clone() {
                    bundle_patch.dynamic_patch(dynamic_scene);
                }
                dynamic_scene.hot_related = Some(new_root.related.clone());
            }

            // Recursively arrange the deferred children in their (possibly) new spots and push any new scenes
            let hot_deferred_arc = dynamic_scene.hot_deferred.take().unwrap();
            let mut hot_deferred = hot_deferred_arc.lock().unwrap();
            Self::arrange_children(&mut hot_deferred, dynamic_scene);

            // Restore the deferred for the above invocation
            dynamic_scene.hot_deferred = previous_deferred;
        }
    }

    fn dynamic_patch_original<I, P>(
        hot_id: EntityPatchId,
        inherit: I,
        patch: P,
        hot_related: HotRelated,
        dynamic_scene: &mut DynamicScene,
    ) where
        I: Scene,
        P: Patch + DynamicPatch,
    {
        // Apply the inherited patches
        inherit.dynamic_patch(dynamic_scene);

        // Apply this patch itself
        patch.dynamic_patch(dynamic_scene);

        // Store the new children on the dynamic_scene
        // TODO: Just keep this in the root hot patch?
        dynamic_scene.hot_related = Some(hot_related);

        // Set the hot_id on the dynamic scene so this entity patch can be identified for deferring
        dynamic_scene.hot_id = Some(hot_id);
    }

    fn arrange_children(
        hot_deferred: &mut HashMap<usize, DynamicScene>,
        dynamic_scene: &mut DynamicScene,
    ) {
        // Push any deferred children to the dynamic scene
        if let Some(new_children) = dynamic_scene
            .hot_related
            .as_mut()
            .and_then(|hot_related| hot_related.remove(&TypeId::of::<Children>()))
        {
            for hot_item in new_children.clone().iter() {
                match hot_item {
                    HotItem::OriginalEntity(original_index) => {
                        let deferred = hot_deferred
                            .remove(original_index)
                            .or_else(|| {
                                error!(
                                    "deferred not found: {}\n{:?}",
                                    original_index, hot_deferred
                                );
                                None
                            })
                            .unwrap();

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

        // Initialize (inactive) hot patches for each entity
        let mut hot_patches = HashMap::with_capacity_and_hasher(bsn.entities.len(), FixedHasher);
        for index in bsn.entities.iter().map(|entity| entity.entity_index) {
            hot_patches.insert(index, HotPatch::default());
        }

        Ok(Self {
            original_hash: macro_hash,
            current_hash: macro_hash,
            original_bsn: bsn,
            hot_patches,
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

        // Update the current hash and return if back to original
        self.current_hash = macro_hash;
        if self.current_hash == self.original_hash {
            return Ok(());
        }

        // Parse the new bsn
        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Bsn::from(&bsn_ast);
        let reflector = BsnReflector::new(&bsn, registry);

        // Set all hot patches to inactive
        for hot_patch in self.hot_patches.values_mut() {
            hot_patch.active = false;
        }

        // Diff and reload the new bsn recursively
        self.burned_original_indices.clear();
        let root_item = self.reload_entity(&bsn.root, &reflector);

        // Replace root patch if needed
        let root_hot_patch = self.hot_patches.entry(0).or_default();
        if let HotItem::NewEntity(replaced_root) = root_item {
            root_hot_patch.new_root = Some(Arc::new(replaced_root));
        } else {
            root_hot_patch.new_root = None;
        }

        Ok(())
    }

    fn reload_entity(&mut self, entity: &BsnEntity, reflector: &BsnReflector) -> HotItem {
        if let Some(matching_index) = self.find_original_match(entity) {
            // Found a match, update the hot patch for this entity
            self.burned_original_indices.insert(matching_index);

            // Diff/reload children
            let children = self.reload_children(entity, reflector);

            // Get or create the hot patch for this entity
            let hot_patch = self.hot_patches.entry(matching_index).or_default();
            hot_patch.active = true;

            // Insert children
            if let Some(children) = children {
                hot_patch
                    .related
                    .insert(TypeId::of::<Children>(), Arc::new(children));
            } else {
                hot_patch.related.remove(&TypeId::of::<Children>());
            }

            return HotItem::OriginalEntity(matching_index);
        }

        // No match found, create a new hot patch for this entity
        let mut hot_patch = HotPatch {
            active: true,
            has_bundle_changes: true,
            bundle_patch: Some(HotBundlePatch::new(&entity.components, reflector)),
            ..Default::default()
        };

        // Diff/reload children
        if let Some(children) = self.reload_children(entity, reflector) {
            hot_patch
                .related
                .insert(TypeId::of::<Children>(), Arc::new(children));
        }

        HotItem::NewEntity(hot_patch)
    }

    fn find_original_match(&self, entity: &BsnEntity) -> Option<usize> {
        // Check for components match
        let components_hash = hash(&entity.components);
        if let Some(original_index) = self
            .original_bsn
            .first_entity_with_components_hash(components_hash, &self.burned_original_indices)
            .map(|original_entity| original_entity.entity_index)
        {
            return Some(original_index);
        }

        // TODO: Check for non-reflectable component/field match to allow patching of partially non-reflectable entities/components

        None
    }

    fn reload_children(
        &mut self,
        entity: &BsnEntity,
        reflector: &BsnReflector,
    ) -> Option<Vec<HotItem>> {
        if entity.children.is_empty() {
            return None;
        }

        Some(
            entity
                .children
                .iter()
                .map(|child| self.reload_entity(child, reflector))
                .collect(),
        )
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

        let hot_patch = self
            .hot_patches
            .get_mut(&entity_index)
            .ok_or_else(|| {
                error!("missing hot patch for entity index: {}", entity_index);
            })
            .unwrap();

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
        let mut components_hash_to_entity_index = HashMap::with_hasher(NoOpHash);
        let mut descendant_stack = Vec::new();

        fn recurse(
            entity: BsnEntity,
            flat_entities: &mut Vec<OriginalBsnEntity>,
            components_hash_to_entity_index: &mut HashMap<u64, Vec<usize>, NoOpHash>,
            descendant_stack: &mut Vec<usize>,
        ) {
            let flat_entity = OriginalBsnEntity {
                entity_index: flat_entities.len(),
                components_hash: hash(&entity.components),
                components: entity.components,
                //children_hash: hash(&entity.children),
                children: Vec::new(),
                descendants: Vec::new(),
                //key: entity.key,
            };

            let entity_index = flat_entity.entity_index;

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
            &mut components_hash_to_entity_index,
            &mut descendant_stack,
        );

        Self {
            entities: flat_entities,
            components_hash_to_entity_index,
        }
    }

    fn get_entity_by_index(&self, index: usize) -> Option<&OriginalBsnEntity> {
        self.entities.get(index)
    }

    fn iter_indices_by_components_hash(&self, hash: u64) -> impl Iterator<Item = usize> + '_ {
        self.components_hash_to_entity_index
            .get(&hash)
            .into_iter()
            .flat_map(|v| v.iter().copied())
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
