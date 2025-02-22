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

use crate::{Bsn, BsnReflector, DynamicPatch, DynamicScene, Patch, Scene};

use super::hash;

/// A patch to apply hot-reloaded changes to a [`DynamicScene`].
#[derive(Debug, Clone)]
pub struct HotPatch {
    component_patches: Arc<TypeIdMap<Arc<dyn PartialReflect>>>,
    removed_components: Arc<HashSet<TypeId, NoOpHash>>,
    // TODO: Children
}

// impl DynamicPatch for HotPatch {
//     fn dynamic_patch(self, scene: &mut DynamicScene) {
//         for (type_id, patch) in self.component_patches.iter() {
//             let patch = patch.clone();
//             scene.patch_reflected(*type_id, move |props: &mut dyn PartialReflect| {
//                 props.apply(patch.as_ref());
//             });
//         }

//         for type_id in self.removed_components.iter() {
//             scene.remove_component(*type_id);
//         }
//     }
// }

impl HotPatch {
    fn new_diffed(registry: &TypeRegistry, invocation: &BsnInvocation) -> HotPatch {
        let bsn = &invocation.current_bsn;
        let reflector = BsnReflector::new(bsn, registry);

        let original_components = &invocation.original_components();
        let component_count = bsn.root.components.len();

        // Add patches for new or changed components
        let mut component_patches = TypeIdMap::with_capacity_and_hasher(component_count, NoOpHash);
        let mut keep_components = HashSet::with_capacity_and_hasher(component_count, NoOpHash);
        for bsn_component in bsn.root.components.iter() {
            // If there is a matching component in original_components (by hash), keep it.
            let component_hash = hash(bsn_component);
            if let Some(original_component) = original_components.get_by_hash(component_hash) {
                keep_components.insert(original_component.type_id);
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
        for original_component in original_components.iter() {
            if !keep_components.contains(&original_component.type_id) {
                debug!("Component removed: {:?}", original_component.type_id);
                removed_components.insert(original_component.type_id);
            }
        }

        debug!(
            "Hot patch diffed\n Original components:\n{:?} \n\nPatches: {:?}\n\nRemoved: {:?}",
            original_components, component_patches, removed_components
        );

        Self {
            component_patches: Arc::new(component_patches),
            removed_components: Arc::new(removed_components),
        }
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
        for (type_id, patch) in self.component_patches.iter() {
            let patch = patch.clone();
            dynamic_scene.patch_reflected(*type_id, move |props: &mut dyn PartialReflect| {
                props.apply(patch.as_ref());
            });
        }
        for type_id in self.removed_components.iter() {
            dynamic_scene.remove_component(*type_id);
        }

        // Push the children
        children.dynamic_patch_as_children(dynamic_scene);

        // TODO: Children
    }
}

#[derive(Debug)]
struct OriginalComponent {
    //index: usize,
    type_id: TypeId,
    //hash: u64,
}

#[derive(Debug)]
struct OriginalComponents {
    components: Vec<OriginalComponent>,
    indices_by_hash: HashMap<u64, usize, NoOpHash>,
    //indices_by_type_id: TypeIdMap<usize>,
}

impl OriginalComponents {
    fn new<B: Bundle>(original_bsn: &Bsn, world: &World) -> Option<Self> {
        let size_hint = original_bsn.root.components.len();
        let mut original_components = Vec::with_capacity(size_hint);
        let mut indices_by_hash = HashMap::with_capacity_and_hasher(size_hint, NoOpHash);
        let mut indices_by_type_id = TypeIdMap::with_capacity_and_hasher(size_hint, NoOpHash);

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

            let hash = hash(&original_bsn.root.components[i]);

            original_components.push(OriginalComponent {
                //index: i,
                type_id,
                //hash,
            });
            indices_by_hash.insert(hash, i);
            indices_by_type_id.insert(type_id, i);

            i += 1;
        });

        if has_unregistered {
            return None;
        }

        Some(Self {
            components: original_components,
            indices_by_hash,
            //indices_by_type_id,
        })
    }

    fn iter(&self) -> impl Iterator<Item = &OriginalComponent> {
        self.components.iter()
    }

    fn get_unchecked(&self, index: usize) -> &OriginalComponent {
        &self.components[index]
    }

    fn get_by_hash(&self, hash: u64) -> Option<&OriginalComponent> {
        self.indices_by_hash
            .get(&hash)
            .map(|i| self.get_unchecked(*i))
    }

    // fn get_by_type_id(&self, type_id: TypeId) -> Option<&OriginalComponent> {
    //     self.indices_by_type_id
    //         .get(&type_id)
    //         .map(|i| self.get_unchecked(*i))
    // }
}

/// Holds information about a bsn!-macro invocation for hot-reloading.
#[derive(Debug)]
pub struct BsnInvocation {
    original_hash: u64,
    current_hash: u64,

    original_bsn: Arc<Bsn>,
    original_components: Option<OriginalComponents>,
    current_bsn: Arc<Bsn>,

    hot_patch: Option<HotPatch>,
    // TODO: Children
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
            original_components: None,
            original_bsn: bsn.clone(),
            current_bsn: bsn,
            hot_patch: None,
        })
    }

    /// Update this invocation with the changes from a reloaded macro invocation.
    pub fn reload_invocation(&mut self, macro_ast: &syn::Macro) -> Result<(), syn::Error> {
        let macro_hash = hash(macro_ast);
        if macro_hash == self.current_hash {
            return Ok(());
        }
        self.current_hash = macro_hash;

        if macro_hash == self.original_hash {
            self.reset_to_original();
            return Ok(());
        }

        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Bsn::from(&bsn_ast);

        self.current_bsn = Arc::new(bsn);

        // Reset the hot patch as it is now outdated. It will be re-diffed on next use.
        self.hot_patch = None;

        Ok(())
    }

    /// Prepares a [`crate::EntityPatch`] for hot patching. This should be called before calling [`crate::DynamicPatch::dynamic_patch`].
    ///
    /// Returns a [`HotPatch`] if there are any changes from the original patch.
    #[inline]
    pub fn init_hot_patch<I, P, C>(&mut self, world: &World) -> Option<HotPatch>
    where
        I: Scene,
        P: Patch,
        C: Scene,
    {
        if !self.has_changes() {
            return None;
        }

        // Initialize the original components using static [`Bundle`] information if not already done.
        if self.original_components.is_none() {
            self.original_components =
                OriginalComponents::new::<P::Construct>(&self.original_bsn, world);
            if self.original_components.is_none() {
                warn!(
                    "Hot-patching failed due to unregistered components. Try saving the file again."
                );
                self.reset_to_original();
                return None;
            }
        }

        // Diff and update the hot patch if needed.
        if self.hot_patch.is_none() {
            let registry = world.resource::<AppTypeRegistry>().read();
            self.hot_patch = Some(HotPatch::new_diffed(&registry, self));
        }

        self.hot_patch.clone()
    }

    fn original_components(&self) -> &OriginalComponents {
        self.original_components
            .as_ref()
            .expect("Original components not initialized")
    }

    fn reset_to_original(&mut self) {
        self.current_hash = self.original_hash;
        self.current_bsn = self.original_bsn.clone();
        self.hot_patch = None;
    }
}
