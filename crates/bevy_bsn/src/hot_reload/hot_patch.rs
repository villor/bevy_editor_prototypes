use std::{any::TypeId, sync::Arc};

use bevy::{
    ecs::{bundle::Bundle, reflect::AppTypeRegistry, world::World},
    log::{debug, warn},
    platform_support::{
        collections::{HashMap, HashSet},
        hash::NoOpHash,
    },
    reflect::{PartialReflect, TypeRegistry},
    utils::TypeIdMap,
};
use bevy_bsn_ast::{syn, BsnAstEntity};

use crate::{Bsn, BsnEntity, BsnReflector, DynamicPatch, DynamicScene, EntityPatch, Patch, Scene};

use super::hash;

/// The bundle/component part of a hot patch.
#[derive(Debug, Clone)]
struct HotBundlePatch {
    hash: u64,
    component_patches: Arc<TypeIdMap<Arc<dyn PartialReflect>>>,
    removed_components: Arc<HashSet<TypeId, NoOpHash>>,
}

impl HotBundlePatch {
    fn reload(
        entity: &BsnEntity,
        original_bundle: &OriginalBundle,
        reflector: &BsnReflector,
        previous: Option<HotBundlePatch>,
    ) -> Self {
        let bundle_hash = hash(&entity.components);
        if let Some(previous) = previous {
            if bundle_hash == previous.hash {
                return previous;
            }
        }

        // Add patches for new or changed components
        let component_count = entity.components.len();
        let mut component_patches = TypeIdMap::with_capacity_and_hasher(component_count, NoOpHash);
        let mut keep_components = HashSet::with_capacity_and_hasher(component_count, NoOpHash);
        for bsn_component in entity.components.iter() {
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

                    component_patches.insert(reflected.type_id, reflected.props.instance.into());
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

/// A patch to apply hot-reloaded changes to a [`DynamicScene`].
#[derive(Debug, Clone)]
pub struct HotPatch {
    bundle_patch: HotBundlePatch,
    // TODO: Children
}

impl HotPatch {
    fn reload(
        registry: &TypeRegistry,
        original_bundle: &OriginalBundle,
        bsn: &Bsn,
        bsn_entity: &BsnEntity,
        previous: Option<HotPatch>,
    ) -> HotPatch {
        let reflector = BsnReflector::new(bsn, registry);
        let bundle_patch = HotBundlePatch::reload(
            bsn_entity,
            original_bundle,
            &reflector,
            previous.map(|p| p.bundle_patch),
        );
        Self { bundle_patch }
    }

    /// Hook that replaces `dynamic_patch` for an [`EntityPatch`] with a hot patch.
    #[inline]
    pub fn hot_hook_entity_patch<I, P, C>(
        self,
        inherit: I,
        patch: P,
        children: C,
        dynamic_scene: &mut DynamicScene,
    ) where
        I: Scene,
        P: Patch + DynamicPatch,
        C: Scene,
    {
        // Apply the inherited patches
        // TODO: Support hot-reloading changes to inheritance list
        inherit.dynamic_patch(dynamic_scene);

        // Apply this patch itself
        patch.dynamic_patch(dynamic_scene);

        // Apply the hot patch
        for (type_id, patch) in self.bundle_patch.component_patches.iter() {
            let patch = patch.clone();
            dynamic_scene.patch_reflected(*type_id, move |props: &mut dyn PartialReflect| {
                props.apply(patch.as_ref());
            });
        }
        for type_id in self.bundle_patch.removed_components.iter() {
            dynamic_scene.remove_component(*type_id);
        }

        // Push the children
        children.dynamic_patch_as_children(dynamic_scene);

        // TODO: Children
    }
}

#[derive(Debug)]
struct OriginalBundle {
    type_ids: Vec<TypeId>,
    indices_by_hash: HashMap<u64, usize, NoOpHash>,
}

impl OriginalBundle {
    fn new<B: Bundle>(original_entity: &BsnEntity, world: &World) -> Option<Self> {
        let size_hint = original_entity.components.len();
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

            let hash = hash(&original_entity.components[i]);

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

/// Holds information about a bsn!-macro invocation for hot-reloading.
#[derive(Debug)]
pub struct BsnInvocation {
    original_hash: u64,
    current_hash: u64,

    original_bsn: Arc<Bsn>,
    original_bundles: HashMap<usize, OriginalBundle>,
    current_bsn: Arc<Bsn>,

    previous_hot_patches: HashMap<usize, HotPatch>,
    hot_patches: HashMap<usize, HotPatch>,
}

impl BsnInvocation {
    /// Returns true if this invocation has any reloaded changes from the original macro.
    pub fn has_changes(&self) -> bool {
        self.original_hash != self.current_hash
    }

    /// Tries to parse and reflect a [`BsnInvocation`] from an original bsn! macro invocation.
    pub fn try_from_original(macro_ast: &syn::Macro) -> Result<Self, syn::Error> {
        let macro_hash = hash(macro_ast);
        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Arc::new(Bsn::from(&bsn_ast));

        Ok(Self {
            original_hash: macro_hash,
            current_hash: macro_hash,
            original_bundles: Default::default(),
            original_bsn: bsn.clone(),
            current_bsn: bsn,
            previous_hot_patches: Default::default(),
            hot_patches: Default::default(),
        })
    }

    /// Update this invocation with the changes from a reloaded macro invocation.
    pub fn reload_invocation(&mut self, macro_ast: &syn::Macro) -> Result<(), syn::Error> {
        let macro_hash = hash(macro_ast);
        if macro_hash == self.current_hash {
            return Ok(());
        }

        if macro_hash == self.original_hash {
            self.reset_to_original();
            return Ok(());
        }

        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Bsn::from(&bsn_ast);

        self.current_bsn = Arc::new(bsn);
        self.current_hash = macro_hash;

        // Reset the hot patch as it is now outdated. It will be re-diffed on next use.
        self.previous_hot_patches = core::mem::take(&mut self.hot_patches);

        Ok(())
    }

    /// Prepares a bsn!-invocation for hot patching.
    ///
    /// This should only be called before any call to [`crate::DynamicPatch::dynamic_patch`].
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

        let entity_index = entity_patch.entity_index;

        // Initialize the original bundle info using [`Bundle`] if not already done.
        if !self.original_bundles.contains_key(&entity_index) {
            let entity = find_bsn_entity_by_index(&self.original_bsn, entity_index)
                .expect("entity index mismatch");

            let Some(original_bundle) = OriginalBundle::new::<P::Construct>(entity, world) else {
                warn!(
                    "Some changes could not be hot-reloaded due to unregistered components. Try saving the file again."
                );
                return;
            };

            self.original_bundles.insert(entity_index, original_bundle);
        }

        // Diff and update the hot patch if needed
        let hot_patch = self.hot_patches.entry(entity_index).or_insert_with(|| {
            let registry = world.resource::<AppTypeRegistry>().read();

            let bsn_entity = find_bsn_entity_by_index(&self.current_bsn, entity_index)
                .expect("entity index mismatch");

            HotPatch::reload(
                &registry,
                self.original_bundles.get(&entity_index).unwrap(),
                &self.current_bsn,
                bsn_entity,
                self.previous_hot_patches.get(&entity_index).cloned(),
            )
        });

        entity_patch.hot_patch = Some(hot_patch.clone());
    }

    fn reset_to_original(&mut self) {
        self.current_hash = self.original_hash;
        self.current_bsn = self.original_bsn.clone();
        self.previous_hot_patches = core::mem::take(&mut self.hot_patches);
    }
}

fn find_bsn_entity_by_index(bsn: &Bsn, id: usize) -> Option<&BsnEntity> {
    if id == 0 {
        return Some(&bsn.root);
    }

    fn recurse<'a>(
        entity: &'a BsnEntity,
        needle: usize,
        current_id: &mut usize,
    ) -> Option<&'a BsnEntity> {
        for child in entity.children.iter() {
            if *current_id == needle {
                return Some(child);
            }
            *current_id += 1;

            if let Some(descendant) = recurse(child, needle, current_id) {
                return Some(descendant);
            }
        }

        None
    }

    let mut current_id = 1;
    recurse(&bsn.root, id, &mut current_id)
}
